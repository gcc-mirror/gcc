package Opt18 is

   type Cart_Axis_Type is (X, Y, Z);

   type Cart_Vector_Type is array (Cart_Axis_Type) of Float;

   function Mag (Item : in Cart_Vector_Type) return Float;

   type Unit_Vector_Type is array (Cart_Axis_Type) of Float;

   type Mag_Axis_Type is
   record
      Mag  : Float;
      Axis : Unit_Vector_Type;
   end record;

   type Unit_Quaternion_Type is record
      X : Float;
      Y : Float;
      Z : Float;
      S : Float;
   end record;

   function Unit_Quaternion_To_Mag_Axis (Quaternion : in Unit_Quaternion_Type)
     return Mag_Axis_Type;

   X_Unit : constant Unit_Vector_Type := (1.0, 0.0, 0.0);

end Opt18;
