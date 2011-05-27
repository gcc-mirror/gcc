package Bit_Packed_Array5 is

   type Bit_Array is array (Integer range <>) of Boolean;
   pragma Pack (Bit_Array);

   type Short_Bit_Array_Type is new Bit_Array (0 .. 15);
   for Short_Bit_Array_Type'Size use 16;

   type Word_Type is range 0 .. 65535;
   for Word_Type'Size use 16;

   function Inv (Word : Word_Type) return Word_Type;

end Bit_Packed_Array5;
