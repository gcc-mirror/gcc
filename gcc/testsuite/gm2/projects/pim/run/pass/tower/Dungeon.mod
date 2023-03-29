MODULE Dungeon ;
(* *)

FROM AdvIntroduction IMPORT StartGame ;
FROM SArgs IMPORT GetArg ;
FROM AdvParse IMPORT ParseMap ;
FROM DynamicStrings IMPORT String, string ;
FROM libc IMPORT printf, exit ;
FROM Screen IMPORT AssignMapName ;


VAR
   s: String ;
   r: INTEGER ;
BEGIN
   IF GetArg(s, 1)
   THEN
      r := ParseMap(string(s)) ;
      IF r=0
      THEN
         AssignMapName(s) ;
         StartGame
      ELSE
         exit(r)
      END
   ELSE
      r := printf("usage: dungeon mapfile\n")
   END
END Dungeon.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
