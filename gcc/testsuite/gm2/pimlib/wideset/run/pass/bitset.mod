MODULE bitset ;

FROM libc IMPORT printf, exit ;
FROM M2WIDESET IMPORT Equal, Clear ;

TYPE
   set = BITSET ;

CONST
   HighBit = MAX (set) ;


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
   left, right: set ;
BEGIN
   left := set {} ;
   right := set {1} ;
   Assert (NOT Equal (left, right, HighBit), __LINE__) ;
   Clear (right, HighBit) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;
   printf ("All tests pass in %s\n", __FILE__)
END init ;


BEGIN
   init
END bitset.
