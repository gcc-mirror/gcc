--  { dg-do compile }

package body Incomplete6 is

   function "=" (Left, Right : Vint) return Boolean is
   begin
      return Left.Value = Right.Value;
   end;
   
   function "=" (Left, Right : Vfloat) return Boolean is
   begin
      return Left.Value = Right.Value;
   end;

end;
