MODULE constbool5 ;

FROM libc IMPORT printf, exit ;

CONST
   World = "W" + "o" + "r" + "l" + "d" ;
   Hello = "Hello" + " " + World ;
   AddressableBits = 32 ;
   MaxBits         = 32 ;

   BitsInUse =
    ORD(AddressableBits > MaxBits) * MaxBits +
    ORD(AddressableBits <= MaxBits) * AddressableBits +
    ORD (LENGTH (Hello) = 11) ;

BEGIN
   IF BitsInUse = 33
   THEN
      printf ("passed\n") ;
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END constbool5.
