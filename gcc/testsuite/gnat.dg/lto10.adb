-- { dg-do run }
-- { dg-options "-flto" { target lto } }

with Lto10_Pkg; use Lto10_Pkg;

procedure Lto10 is
   A : Integer := Minus_One;
   Pos : Position;
begin
   Pos := Pix.Pos;
   if A /= Minus_One then
      raise Program_Error;
   end if;
end;
