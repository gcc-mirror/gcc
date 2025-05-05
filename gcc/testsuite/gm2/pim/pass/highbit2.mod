MODULE highbit2 ;

FROM libc IMPORT printf ;

TYPE
   set = BITSET ;

CONST
   HighBit = MAX (BITSET) ;

BEGIN
   printf ("the MAX (BITSET) = %d\n", HighBit)
END highbit2.
