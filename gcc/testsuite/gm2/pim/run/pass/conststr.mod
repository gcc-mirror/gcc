MODULE conststr ;

FROM StrLib IMPORT StrLen ;
FROM libc IMPORT printf, exit ;

VAR
   r: INTEGER ;
BEGIN
   r := 0 ;
   IF StrLen ("\n") # 2
   THEN
      printf ("\\n string should be 2 characters long in Modula-2\n") ;
      r := 1
   END ;
   IF StrLen ("\\n") # 3
   THEN
      printf ("\\\\n string should be 3 characters long in Modula-2\n") ;
      r := 2
   END ;
   IF r = 0
   THEN
      printf ("very basic escaped strings pass\n") ;
   END ;
   exit (r)
END conststr.
