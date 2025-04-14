(* { dg-do assemble { target { x86_64-*-* } } } *)
(* { dg-options "-g" } *)

MODULE exampleadd2 ;  

FROM libc IMPORT printf, exit ;


PROCEDURE Example (foo, bar: CARDINAL) : CARDINAL ;
VAR
   myout: CARDINAL ;
BEGIN
   ASM VOLATILE (
    "movl %[left],%%eax; addl %[right],%%eax; movl %%eax,%[output]"
      : [output] "=rm" (myout)                  (* outputs *)
      : [left] "rm" (foo), [right] "rm" (bar)   (* inputs  *)
      : "eax") ;                                (* we trash *)
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
END exampleadd2.
