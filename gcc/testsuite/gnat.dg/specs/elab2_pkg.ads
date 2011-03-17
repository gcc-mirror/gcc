-- { dg-excess-errors "no code generated" }

package Elab2_Pkg is

   function Get_Value (S : String) return Integer;

   Max_Limit : constant array(1..2) of Integer :=
     (1 => Get_Value ("One"), 2 => Get_Value ("Two"));

   type Index_Type is new Natural range 0 .. Max_Limit(1);

   type Array_Type is array (Index_Type range <>) of Natural;

   type Rec1(D : Index_Type) is record
      A : Array_Type(1 .. D);
   end record;

end Elab2_Pkg;
