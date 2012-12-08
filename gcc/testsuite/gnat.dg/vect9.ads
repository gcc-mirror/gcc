with Vect9_Pkg; use Vect9_Pkg;

package Vect9 is

   type Rec is record
      Data : Vector_Access;
   end record;

   procedure Proc
     (This : in Rec;
      CV   : in Unit_Vector;
      Data : in out Unit_Vector);

end Vect9;
