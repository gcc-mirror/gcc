MODULE constbool3 ;


CONST
   AddressableBits = 32 ;
   MaxBits         = 16 ;

   BitsInUse = ORD(AddressableBits > MaxBits) * MaxBits + ORD(AddressableBits <= MaxBits) * AddressableBits;

BEGIN

END constbool3.
