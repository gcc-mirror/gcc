--  { dg-do run }
--  { dg-options "-gnatws" }

with discr3; use discr3;
with Text_IO; use Text_IO;
procedure Conv_Bug is
begin
   begin
      V2 := S2 (V1);
   exception
      when Constraint_Error => null;
      when others => Put_Line ("Wrong Exception raised");
   end;
   
   begin
      V2 := S2(V1(V1'Range));
      Put_Line ("No exception raised - 2");
   exception
      when Constraint_Error => null;
      when others => Put_Line ("Wrong Exception raised");
   end;
   
   begin
      V2 := S2 (V3);
      Put_Line ("No exception raised - 3");
   exception
      when Constraint_Error => null;
      when others => Put_Line ("Wrong Exception raised");
   end;
end Conv_Bug;
