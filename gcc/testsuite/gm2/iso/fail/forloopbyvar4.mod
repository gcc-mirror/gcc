MODULE forloopbyvar4 ;

PROCEDURE TestFor (boolarray: ARRAY OF BOOLEAN);
VAR
   k, m: CARDINAL ;
BEGIN
   k := 4 ;
   FOR m := k * k TO HIGH (boolarray) BY k DO
      boolarray[m] := FALSE;
   END
END TestFor ;

VAR
   boolarray: ARRAY [1..1024] OF BOOLEAN ;
BEGIN
   TestFor (boolarray)
END forloopbyvar4.
