-- { dg-do compile }
-- { dg-options "-O3" }

with Opt18_Pkg; use Opt18_Pkg;

package body Opt18 is

   function Mag (Item : in Cart_Vector_Type) return Float is
   begin
      return Sqrt (Item (X) * Item (X) + Item (Y) * Item (Y)
                   + Item (Z) * Item (Z));
   end;

   function Unit_Quaternion_To_Mag_Axis (Quaternion : in Unit_Quaternion_Type)
   return Mag_Axis_Type is
      Sin_Half : Float
         := Mag (Cart_Vector_Type'(Quaternion.X, Quaternion.Y, Quaternion.Z));
   begin
      if Sin_Half > 3.0 * First_Order_Trig then
         return
            (Mag  => Atan2 (Double_Trig (Unchecked_Trig_Pair (Sin_Half,
                                                              Quaternion.S))),
             Axis => Unit_Vector_Type'(Quaternion.X / Sin_Half,
                                       Quaternion.Y / Sin_Half,
                                       Quaternion.Z / Sin_Half));
      else
         return (0.0, X_Unit);
      end if;
   end;

end Opt18;
