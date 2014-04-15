package Vect13 is

   -- Constrained array types are vectorizable
   type Sarray is array (1 .. 4) of Float;
   for Sarray'Alignment use 16;

   function "+" (X, Y : Sarray) return Sarray;
   procedure Add (X, Y : Sarray; R : out Sarray);

end Vect13;
