MODULE hightests ;


FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrCopy ;

PROCEDURE testhigh (a: ARRAY OF CHAR; expected: CARDINAL; first: CHAR; checkNul: BOOLEAN) ;
VAR
   copy: ARRAY [0..10] OF CHAR ;
BEGIN
   StrCopy (a, copy) ;
   IF HIGH (a) # expected
   THEN
      printf ("unexpected high value, HIGH(%s) should be %d but was passed %d\n",
              copy, expected, HIGH (a)) ;
      code := 1
   END ;
   IF a[0] # first
   THEN
      printf ("unexpected first value in open array, %s, a[0] should be %c but was passed %c\n",
              a, first, a[0]) ;
      code := 2
   END ;
   IF checkNul AND (a[HIGH(a)] # 0C)
   THEN
      printf ("expected the array to contain a 0C terminator\n") ;
      code := 3
   END
END testhigh ;


VAR
   str0: ARRAY [0..0] OF CHAR ;
   str1: ARRAY [0..1] OF CHAR ;
   str2: ARRAY [0..2] OF CHAR ;
   str3: ARRAY [0..3] OF CHAR ;
   ch  : CHAR ;
   code: INTEGER ;
BEGIN
   testhigh ('1', 1, '1', TRUE) ;
   str0 := '_' ;
   str1 := '_1' ;
   str2 := '_2' ;
   str3 := '_3' ;
   code := 0 ;
   testhigh ('', 0, 0C, TRUE) ;
   testhigh ('1', 1, '1', TRUE) ;
   testhigh ('12', 2, '1', TRUE) ;
   testhigh ('123', 3, '1', TRUE) ;
   testhigh ('1234', 4, '1', TRUE) ;
   testhigh (str0, 0, '_', FALSE) ;
   testhigh (str1, 1, '_', FALSE) ;
   testhigh (str2, 2, '_', TRUE) ;
   testhigh (str3, 3, '_', TRUE) ;
   IF code = 0
   THEN
      printf ("all tests pass\n")
   ELSE
      exit (1)
   END
END hightests.
