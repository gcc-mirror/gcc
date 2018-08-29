--  { dg-do run }

with Text_IO; use Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Unchecked_Conversion;

procedure Fixedpnt5 is
   --  Test conversions from Floating point to Fixed point types when the
   --  fixed type has a Small that is not a power of two. Verify that the
   --  conversions are reversible, so that:
   --
   --        Fixed_T (Float_T (Fixed_Var)) = Fixed_Var
   --
   --  for a range of fixed values, in particular at the boundary of type.

   type T_Fixed_Type is delta PI/32768.0 range -PI .. PI - PI/32768.0;
   for T_Fixed_Type'Small use PI/32768.0;

   function To_Fix is new Unchecked_Conversion (Short_Integer, T_Fixed_Type);
   Fixed_Point_Var : T_Fixed_Type;
   Float_Var       : Float;

begin
   Fixed_Point_Var := -PI;
   Float_Var       := Float(Fixed_Point_Var);
   Fixed_Point_Var := T_Fixed_Type (Float_Var);

   Fixed_Point_Var := T_Fixed_Type'First;
   Float_Var       := Float(Fixed_Point_Var);
   Fixed_Point_Var := T_Fixed_Type (Float_Var);

   if Fixed_Point_Var /= T_Fixed_Type'First then
      raise Program_Error;
   end if;

   fixed_point_var := t_fixed_type'Last;
   Float_Var       := Float(Fixed_Point_Var);
   Fixed_Point_Var := T_Fixed_Type (Float_Var);

   if Fixed_Point_Var /= T_Fixed_Type'Last then
      raise Program_Error;
   end if;

   for I in  -32768 ..  32767 loop
      fixed_Point_Var := To_Fix (Short_Integer (I));
      Float_Var := Float (Fixed_Point_Var);
      if T_Fixed_Type (Float_Var) /= FIxed_Point_Var then
         Put_Line ("Not reversibloe");
         Put_Line (Integer'Image (I));
         raise Program_Error;
      end if;
   end loop;

   Fixed_Point_Var := T_Fixed_Type (Float_Var * 2.0);
   raise Program_Error;
exception
   when others => null;
end Fixedpnt5;
