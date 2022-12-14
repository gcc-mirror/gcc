(* testlibc a trivial test for the existence of libc.  *)

MODULE testlibc ;


FROM libc IMPORT exit ;

BEGIN
   exit (0)
END testlibc.
