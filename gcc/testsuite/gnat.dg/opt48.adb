-- { dg-do run }
-- { dg-options "-O" }

with Opt48_Pkg1; use Opt48_Pkg1;
with Opt48_Pkg2; use Opt48_Pkg2;

procedure Opt48 is
begin
   if Get_Z /= (12, "Hello world!") then
      raise Program_Error;
   end if;
end;
