use "files"
use "collections"

actor Main 
  let env:Env
  new create(env':Env) =>
    env = env'
    env.out.print("Starting program")
    DirParser(Path.abs("."), env, None, this)


  be print(paths:Array[String] val) =>
    try
      for i in Range(0, paths.size()) do
        env.out.print(paths(i))
      end
    end

actor DirParser
  let main:(Main | None)
  let _found:Array[String] = Array[String] 
  let parsers:Array[DirParser] = Array[DirParser]
  let parent:(DirParser | None)
  let env:Env

  new create(path:String val, env':Env, parent':(DirParser | None) = None, main':(Main | None) = None) => 
    env = env'
    main = main'
    parent = parent'

    let dir:(Directory | None) = recover 
      try 
        let d = Directory(path) 
        consume d 
      else 
        None 
      end 
    end

    let res:Array[String] iso = recover Array[String] end
    
    match dir
    | var d:Directory =>
        let size = d.entries.size()
        try
          for i in Range(0, size) do
            let p = Path.join(path, d.entries(i))
            let f = FileInfo(p)
            if f.directory then
              parsers.push(DirParser(p, env, this))
            else
              if p.reverse().substring(0, 3).reverse().eq(".iml") then
                res.push(p)
              end  
            end
          end
        end
    end
    
    let results:Array[String] val = consume res
    complete(results)

  be complete(paths:Array[String] val) =>
    _found.concat(paths.values())
    //leaf actor, no more child parsers
    if parsers.size() == 0 then
      match parent
      | var p:DirParser => p.finish(this, Utils.clone(_found))
      | None => match main
                | var m:Main => 
                  let finalResult:Array[String] val = Utils.clone(_found) 
                  m.print(consume finalResult)
                end
      end
    end


  be finish(child:DirParser, paths:Array[String] val) =>
    try
      let index = parsers.find(child)
      parsers.delete(index)
      complete(paths)
    end

primitive Utils
  fun clone(src:Array[String]):Array[String] val =>
    let s = src.size()
    let result:Array[String] iso = recover Array[String](s) end
    for value in src.values() do
      result.push(value)
    end
    consume result
