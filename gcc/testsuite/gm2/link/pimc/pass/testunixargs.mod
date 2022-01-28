MODULE testunixargs ;

(* A trivial test to test for the existence of UnixArgs and SYSTEM.  *)

FROM UnixArgs IMPORT ArgV, ArgC ;
FROM SYSTEM IMPORT ADDRESS ;

VAR
   ptr: ADDRESS ;
   num: CARDINAL ;
BEGIN
   ptr := ArgV ;
   num := ArgC
END testunixargs.