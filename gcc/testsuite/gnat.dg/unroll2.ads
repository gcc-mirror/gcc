package Unroll2 is

   type Sarray is array (1 .. 4) of Float;

   function "+" (X, Y : Sarray) return Sarray;
   procedure Add (X, Y : Sarray; R : out Sarray);

end Unroll2;
