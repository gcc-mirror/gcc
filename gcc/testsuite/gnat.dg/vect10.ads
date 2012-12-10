with Vect9_Pkg; use Vect9_Pkg;

package Vect10 is

   type Rec is record
      Val : Unit;
   end record;

   type Rec_Vector is array (Positive range <>) of Rec;

   procedure Proc
     (F            : in Rec_Vector;
      First_Index : in Natural;
      Last_Index  : in Natural;
      Result      : out Unit);

end Vect10;
