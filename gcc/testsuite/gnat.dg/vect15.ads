package Vect15 is

   type Sarray is array (1 .. 4) of Long_Float;
   for Sarray'Alignment use 16;

   procedure Add (X, Y : Sarray; R : out Sarray);

end Vect15;
