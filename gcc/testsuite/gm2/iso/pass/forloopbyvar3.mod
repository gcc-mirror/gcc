MODULE forloopbyvar3 ;

PROCEDURE TestFor (boolarray: ARRAY OF BOOLEAN);
VAR
   m: CARDINAL ;
BEGIN
   FOR m := HIGH (boolarray) TO 2 BY -2 DO
      boolarray[m] := FALSE;
   END
END TestFor ;

VAR
   boolarray: ARRAY [1..1024] OF BOOLEAN ;
BEGIN
   TestFor (boolarray)
END forloopbyvar3.
