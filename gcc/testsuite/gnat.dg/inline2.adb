-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

with Inline2_Pkg; use Inline2_Pkg;

procedure Inline2 is
   F : Float := Invalid_Real;
begin
   if Valid_Real (F) then
      F := F + 1.0;
   end if;
end;
