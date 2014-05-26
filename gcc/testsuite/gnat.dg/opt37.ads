package Opt37 is

   type T_Bit is range 0 .. 1;
   for T_Bit'Size use 1;

   type Positive is range 0 .. (2 ** 31) - 1;
   type Unsigned32 is mod 2 ** 32;

   subtype T_Bit_Count is Positive;
   subtype T_Bit_Index is T_Bit_Count range 1 .. T_Bit_Count'Last;

   type T_Bit_Array is array (T_Bit_Count range <>) of T_Bit;
   pragma Pack (T_Bit_Array);

   function Func (Bit_Array : in T_Bit_Array;
                  Bit_Index : in T_Bit_Index) return Positive;

end Opt37;
