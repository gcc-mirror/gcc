MODULE tinyrange ;

(* This test is useful to check that the runtime system starts the
   application after the runtime.  The import list here is minimal
   (and only from a definition for "C") thus the compiler will not
   see the application in the rest of the modula2 import graph.
   M2Dependent.mod will have to force the application module to the
   end of the initialization ordered list.  *)

FROM libc IMPORT printf ;

VAR
   i: INTEGER ;
BEGIN
   i := MIN (INTEGER) ;
   DEC (i)
END tinyrange.
