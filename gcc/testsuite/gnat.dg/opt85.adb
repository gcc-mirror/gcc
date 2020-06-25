-- { dg-do compile }
-- { dg-options "-O" }

package body Opt85 is

   function Conversion_Of (Value : Integer) return Data_Type is
   begin
      return (Value => Interfaces.Integer_16 (Value));
   end;

   function Create (Value : Integer) return Record_Type is
      Rec : constant Record_Type :=
        (Ada.Finalization.Controlled with
         Header => (others => False),
         Data   => Conversion_Of (Value));
   begin
      return Rec;
   end;

end Opt85;
