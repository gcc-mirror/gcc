package Array17_Pkg is

   type Varray is array (Integer range <>) of Long_Float;
   for Varray'Alignment use 16;

   function "+" (X, Y : Varray) return Varray;

end Array17_Pkg;
