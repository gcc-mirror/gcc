with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;

package Loop_Optimization13 is

   type Complex_Vector is array (Integer range <>) of Complex;
   type Complex_Vector_Ptr is access Complex_Vector;

   type Rec (Kind : Boolean := False) is record
      case Kind is
         when True => V : Complex_Vector_Ptr;
         when False => null;
      end case;
   end record;

   function F (A : Rec) return Rec;

end Loop_Optimization13;
