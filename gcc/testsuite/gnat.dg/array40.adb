-- { dg-do run }
-- { dg-options "-gnatws" }

with System.Storage_Elements;
with Array40_Pkg; use Array40_Pkg;

procedure Array40 is

  use System;
  use System.Storage_Elements;

begin
  if A(1)'Size /= 40 then
    raise Program_Error;
  end if;

  if (A(2)'Address - A(1)'Address) * System.Storage_Unit /= 40 then
    raise Program_Error;
  end if;

end;
