-- { dg-do run }

with assign_from_packed_pixels;
use assign_from_packed_pixels;

procedure assign_from_packed is

   A : Integer := Minus_One;
   Pos : Position;
begin
   Pos := Pix.Pos;
   if A /= Minus_One then
      raise Program_Error;
   end if;
end;
