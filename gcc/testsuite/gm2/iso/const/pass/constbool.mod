MODULE constbool ;


CONST
   AddressableBits = 32 ;
   MaxBits         = 32 ;

   BitsInUse =
    ORD(AddressableBits > MaxBits) * MaxBits +
    ORD(AddressableBits <= MaxBits) * AddressableBits;

BEGIN

END constbool.
