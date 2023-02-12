MODULE conststr2 ;

FROM StrLib IMPORT StrLen ;
FROM libc IMPORT printf, exit ;


TYPE
   opaque = POINTER TO CHAR ;


(*
   local -
*)

PROCEDURE local (p: opaque; a: ARRAY OF CHAR) ;
BEGIN
   IF StrLen (a) # 2
   THEN
      printf ("\\n string should be 2 characters long in Modula-2\n") ;
      r := 1
   END
END local ;


PROCEDURE func (p: opaque; a: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   IF StrLen (a) # 2
   THEN
      printf ("\\n string should be 2 characters long in Modula-2\n") ;
      r := 1
   END ;
   RETURN 2
END func ;


VAR
   r: INTEGER ;
   p: opaque ;
BEGIN
   p := NIL ;
   r := 0 ;
   local (p, "\n") ;
   local (p, '\n') ;
   IF func (p, "\n") # 2
   THEN
      printf ("string escape failed\n") ;
      r := 1
   END ;
   IF r = 0
   THEN
      printf ("very basic escaped strings pass\n")
   END ;
   exit (r)
END conststr2.
