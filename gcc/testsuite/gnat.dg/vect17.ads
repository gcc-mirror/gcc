package Vect17 is

   type Sarray is array (1 .. 5) of Long_Float;
   for Sarray'Alignment use 16;

   procedure Add (X, Y : aliased Sarray; R : aliased out Sarray);

end Vect17;
