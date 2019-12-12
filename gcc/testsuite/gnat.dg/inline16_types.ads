package Inline16_Types with SPARK_Mode is

   type  NvU8 is mod 2 ** 8  with Size => 8;
   type NvU32 is mod 2 ** 32 with Size => 32;

   type Arr_NvU8_Idx32 is array (NvU32 range <>) of NvU8;
end Inline16_Types;
