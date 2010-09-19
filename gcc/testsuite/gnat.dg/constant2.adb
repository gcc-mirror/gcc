-- { dg-do run }
-- { dg-options "-gnatVa" }

with Constant2_Pkg1; use Constant2_Pkg1;

procedure Constant2 is
begin
  if Val then
    raise Program_Error;
  end if;
end;
