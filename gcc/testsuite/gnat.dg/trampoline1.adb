-- { dg-do compile }
-- { dg-options "-gnatws" }

with System; use System;

procedure Trampoline1 is

  A : Integer;

  function F (I : Integer) return Integer is
  begin
    return A + I;
  end F;

  CA : System.Address := F'Code_Address;

begin
  if CA = System.Null_Address then
    raise Program_Error;
  end if;
end;

-- { dg-final { scan-assembler-not "GNU-stack.*x" } }
