package Modular3_Pkg is

   type Int16_T is range -32768 .. 32767;
   for Int16_T'Size use 16;
   for Int16_T'Alignment use 1;

   type Mod16_T is mod 2 ** 16;
   for Mod16_T'Size use 16;
   for Mod16_T'Alignment use 1;

end Modular3_Pkg;
