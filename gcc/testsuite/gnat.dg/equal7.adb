--  { dg-do run }

with Equal7_Pkg; use Equal7_Pkg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Equal7 is
   X : constant Integer := 42;

begin
   if F (X) /= "" & ASCII.LF then
       null;
   end if;
   if not (F (X) = "" & ASCII.LF) then
       null;
   end if;
end;
