with Unroll4_Pkg; use Unroll4_Pkg;

package Unroll4 is

   type Sarray is array (1 .. N) of Float;

   function "+" (X, Y : Sarray) return Sarray;
   procedure Add (X, Y : Sarray; R : out Sarray);

end Unroll4;
