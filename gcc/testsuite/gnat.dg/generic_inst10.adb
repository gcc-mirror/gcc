--  { dg-do compile }

with Generic_Inst10_Pkg; use Generic_Inst10_Pkg;

procedure Generic_Inst10 is

   function Image (S : XString) return String is (S.To_String);

   generic
      type Left_Type (<>) is private;
      type Right_Type (<>) is private;
      with function Image (L : Left_Type) return String is <>;
      with function Image (L : Right_Type) return String is <>;
   procedure G (Left : Left_Type; Right : Right_Type);

   procedure G (Left : Left_Type; Right : Right_Type) is
      A : String := Image (Left) & Image (Right);
   begin
      null;
   end;

   procedure My_G is new G (XString, XString);

begin
   null;
end;
