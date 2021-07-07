MODULE constdynstr ;

FROM DynamicStrings IMPORT String, Length, InitString, KillString ;
FROM libc IMPORT printf, exit ;

VAR
   r: INTEGER ;
   s: String ;
BEGIN
   r := 0 ;
   s := InitString ("\n") ;
   IF Length (s) # 2
   THEN
      printf ("\\n string should be 2 characters long in Modula-2\n") ;
      r := 1
   END ;
   s := KillString (s) ;
   s := InitString ("\\n") ;
   IF Length (s) # 3
   THEN
      printf ("\\\\n string should be 3 characters long in Modula-2\n") ;
      r := 2
   END ;
   IF r = 0
   THEN
      printf ("very basic escaped DynamicStrings pass\n") ;
   END ;
   exit (r)
END constdynstr.
