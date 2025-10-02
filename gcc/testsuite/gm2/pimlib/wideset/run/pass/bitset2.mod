MODULE bitset ;

FROM libc IMPORT printf, exit ;
FROM M2WIDESET IMPORT Equal, Clear ;

CONST
   HighBit = MAX (BITSET) ;


(*
   Assert -
*)

PROCEDURE Assert (bool: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT bool
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line);
      exit (1)
   END
END Assert ;


(*
   init -
*)

PROCEDURE init ;
VAR
   left, right: BITSET ;
BEGIN
   left := BITSET {} ;
   right := BITSET {1} ;
   Assert (NOT Equal (left, right, HighBit), __LINE__) ;
   Clear (right, HighBit) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;
   printf ("All tests pass in %s\n", __FILE__)
END init ;


BEGIN
   init
END bitset.
