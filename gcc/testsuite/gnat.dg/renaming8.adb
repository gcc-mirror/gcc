-- { dg-do run }
-- { dg-options "-gnatp" }

with Renaming8_Pkg1; use Renaming8_Pkg1;

procedure Renaming8 is
begin
  if not B then
    raise Program_Error;
  end if;
end;
