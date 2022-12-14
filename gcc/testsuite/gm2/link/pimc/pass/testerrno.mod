(* testerrno test for the presence of errno.  *)
MODULE testerrno ;


FROM errno IMPORT geterrno ;

VAR
   i: INTEGER ;
BEGIN
   i := geterrno ()
END testerrno.
