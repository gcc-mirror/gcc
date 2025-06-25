(* { dg-do assemble { target { x86_64-*-gnu* } } } *)
(* { dg-options "-masm=intel" } *)
(* { dg-do run { target x86_64-*-gnu* } } *)

MODULE exampleadd2 ;

FROM libc IMPORT printf, exit ;


PROCEDURE Example (foo, bar: LONGCARD) : CARDINAL ;
VAR
   myout: LONGCARD ;
BEGIN
   ASM VOLATILE (
    "mov rax, %[left]; add rax, %[right]; mov %[output], rax;"
      : [output] "=rm" (myout)                  (* outputs *)
      : [left] "rm" (foo), [right] "rm" (bar)   (* inputs  *)
      : "rax") ;                                (* we trash *)
   RETURN( myout )
END Example ;

VAR
   a, b, c: CARDINAL ;
BEGIN
   a := 1 ;
   b := 2 ;
   c := Example (a, b) ;
   IF c = 3
   THEN
      printf ("success result from function is %d\n", c) ;
   ELSE
      printf ("example failed to return 3, seen %d\n" , c) ;
      exit (1)
   END
END exampleadd2.