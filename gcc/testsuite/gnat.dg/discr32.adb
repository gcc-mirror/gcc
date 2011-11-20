-- { dg-do run }
-- { dg-options "-gnatws" }

with Discr32_Pkg; use Discr32_Pkg;

procedure Discr32 is
begin

  if R1'Object_Size /= 32 then
    raise Program_Error;
  end if;

  if R2'Object_Size /= R'Object_Size then
    raise Program_Error;
  end if;

  if R3'Object_Size /= 64 then
    raise Program_Error;
  end if;

end;
