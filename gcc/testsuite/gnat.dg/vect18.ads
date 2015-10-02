package Vect18 is

   type Sarray is array (1 .. 4) of Long_Float;
   for Sarray'Alignment use 16;

   procedure Comp (X, Y : Sarray; R : in out Sarray);

end Vect18;
