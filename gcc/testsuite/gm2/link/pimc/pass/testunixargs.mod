MODULE testunixargs ;

(* A trivial test to test for the existence of UnixArgs and SYSTEM.  *)

FROM UnixArgs IMPORT GetArgV, GetArgC ;
FROM SYSTEM IMPORT ADDRESS ;

VAR
   ptr: ADDRESS ;
   num: CARDINAL ;
BEGIN
   ptr := GetArgV () ;
   num := GetArgC ()
END testunixargs.