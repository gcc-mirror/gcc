MODULE highbit ;

FROM libc IMPORT printf ;

TYPE
   set = BITSET ;

CONST
   HighBit = MAX (set) ;

BEGIN
   printf ("the MAX (set) = %d\n", HighBit)
END highbit.
