package Vect16 is

   type Sarray is array (1 .. 4) of Long_Float;
   for Sarray'Alignment use 16;

   procedure Add_Sub (X, Y : Sarray; R,S : out Sarray);

end Vect16;
