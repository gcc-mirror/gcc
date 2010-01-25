--  { dg-do run }

with Ada.Streams; use Ada.Streams;
procedure Array_Bounds_Test is
    One    : constant Stream_Element := 1;
    Two    : constant Stream_Element := 2;
    Sample : constant Stream_Element_Array := (0 => One) & Two;
begin
   if Sample'First /= 0 then
      raise Program_Error;
   end if;
   if Sample'Last /= 1 then
      raise Program_Error;
   end if;
end Array_Bounds_Test;
