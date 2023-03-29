MODULE minhello ;

(* This test is useful to test the linking to ensure that the
   application module is called after all modules are initialized
   even if the application module only imports from a definition
   for "C".  *)

FROM libc IMPORT printf ;

BEGIN
   printf ("hello world\n")
END minhello.
