(* { dg-do assemble { target { x86_64-*-* } } } *)
(* { dg-options "-g" } *)

MODULE exampleadd ;  

FROM libc IMPORT printf, exit ;


PROCEDURE Example (foo, bar: CARDINAL) : CARDINAL ;
VAR
   myout: CARDINAL ;
BEGIN
   ASM VOLATILE ("movl %1,%%eax; addl %2,%%eax; movl %%eax,%0"
      : "=rm" (myout)            (* outputs *)
      : "rm" (foo), "rm" (bar)   (* inputs  *)
      : "eax") ;                 (* we trash *)
   RETURN( myout )
END Example ;


VAR
   a, b, c: CARDINAL ;
BEGIN
   a := 1 ;
   b := 2 ;
   c := Example (a, b) ;
   IF c # 3
   THEN
      printf ("Example procedure function failed to return 3, seen %d", c) ;
      exit (1)
   END
END exampleadd.
