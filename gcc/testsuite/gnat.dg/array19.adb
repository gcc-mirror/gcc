-- { dg-do compile }

package body Array19 is

   function N return Integer is
   begin
      return 1;
   end;

   type Array_Type is array (1 .. N) of Float;

   type Enum is (One, Two);

   type Rec (D : Enum := Enum'First) is record
      case D is
         when One => null;
         when Two => A : Array_Type;
      end case;
   end record;

   procedure Proc is

      R : Rec;

      function F return Array_Type is
      begin
         return (others => 0.0);
      end F;

   begin
      R.A := F;
   end;

end Array19;
