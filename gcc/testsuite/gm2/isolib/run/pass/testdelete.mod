MODULE testdelete ;

FROM libc IMPORT printf, exit ;
FROM Strings IMPORT Delete, Length ;
FROM StrLib IMPORT StrEqual ;


VAR
   code : INTEGER ;
   one  : ARRAY [0..0] OF CHAR ;
   two  : ARRAY [0..1] OF CHAR ;
   three: ARRAY [0..2] OF CHAR ;
   four : ARRAY [0..3] OF CHAR ;
   large: ARRAY [0..79] OF CHAR ;


PROCEDURE Assert (condition: BOOLEAN; message: ARRAY OF CHAR) ;
BEGIN
   IF NOT condition
   THEN
      printf ("error: %s\n", message) ;
      code := 1
   END
END Assert ;


PROCEDURE stresstest ;
BEGIN
   one := '1' ;
   Delete (one, 0, 1) ;
   printf ("after Delete string one = '%s'\n", one) ;
   Assert (StrEqual (one, ''), 'string one should be empty after delete') ;
   Assert (Length (one) = 0, 'string one have length 0 after delete') ;
   two := '12' ;
   Delete (two, 0, 1) ;
   printf ("after Delete string two = '%s'\n", two) ;
   Assert (StrEqual (two, '2'), "string two should be '2' after delete") ;
   Assert (Length (two) = 1, 'string two have length 1 after delete') ;
   three := '123' ;
   Delete (three, 0, 1) ;
   printf ("after Delete string three = '%s'\n", three) ;
   Assert (StrEqual (three, '23'), "string three should be '23' after delete") ;
   Assert (Length (three) = 2, 'string three should have length 2 after delete') ;
   four := '4' ;
   Delete (four, 0, 1) ;
   printf ("after Delete string four = '%s'\n", four) ;
   Assert (StrEqual (four, ''), "string four should be '' after delete") ;
   Assert (Length (four) = 0, 'string four should have length 0 after delete') ;
   large := '012345678901234567890123456789' ;
   Delete (large, 20, 20) ;
   printf ("after Delete string large = '%s'\n", large) ;
   Assert (StrEqual (large, '01234567890123456789'), "string four should be '01234567890123456789' after delete") ;
   Assert (Length (large) = 20, 'string large should have length 20 after delete') ;

   large := '012345678901234567890123456789' ;
   Delete (large, 10, 10) ;
   printf ("after Delete string large = '%s'\n", large) ;
   Assert (StrEqual (large, '01234567890123456789'), "string four should be '01234567890123456789' after delete") ;
   Assert (Length (large) = 20, 'string large should have length 20 after delete') ;

   three := '123' ;
   Delete (three, 1, 1) ;
   printf ("after Delete string three = '%s'\n", three) ;
   Assert (StrEqual (three, '13'), "string three should be '13' after delete") ;
   Assert (Length (three) = 2, 'string three should have length 2 after delete') ;

   four := '123' ;
   Delete (four, 1, 1) ;
   printf ("after Delete string four = '%s'\n", four) ;
   Assert (StrEqual (four, '13'), "string four should be '13' after delete") ;
   Assert (Length (four) = 2, 'string four should have length 2 after delete') ;

END stresstest ;


BEGIN
   code := 0 ;
   stresstest ;
   IF code = 0
   THEN
      printf ("all tests pass\n")
   ELSE
      exit (code)
   END
END testdelete.