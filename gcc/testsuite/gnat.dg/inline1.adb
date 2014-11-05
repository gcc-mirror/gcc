-- { dg-do compile }

with Inline1_Pkg; use Inline1_Pkg;

procedure Inline1 is
   F : Float := Invalid_Real;
begin
   if Valid_Real (F) then
      F := F + 1.0;
   end if;
end;
