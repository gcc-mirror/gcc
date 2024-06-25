MODULE constbool4 ;


CONST
   World = "W" + "o" + "r" + "l" + "d" ;
   Hello = "Hello" + " " + World ;
   AddressableBits = 32 ;
   MaxBits         = 32 ;

   BitsInUse =
    ORD(AddressableBits > MaxBits) * MaxBits +
    ORD(AddressableBits <= MaxBits) * AddressableBits +
    ORD (LENGTH (Hello) = 15) ;

BEGIN

END constbool4.
