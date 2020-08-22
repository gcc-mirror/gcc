-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with Ada.Command_Line; use Ada.Command_Line;
with Opt86_Pkg; use Opt86_Pkg;

procedure Opt86a is

  S1, S2, S3, S4 : Enum;

begin

  S1 := Enum'Value (Argument (1));
  S2 := Enum'Value (Argument (2));
  S3 := Enum'Value (Argument (3));
  S4 := Enum'Value (Argument (4));

  if S1 in Val2 | Val3 | Val9 then
    raise Program_Error;
  end if;

  if S2 not in Val2 | Val3 | Val9 then
    raise Program_Error;
  end if;

  if S3 = Val1 or else S3 = Val4 or else S3 = Val8 then
    raise Program_Error;
  end if;

  if S4 /= Val1 and S4 /= Val4 and S4 /= Val8 then
    raise Program_Error;
  end if;

end;

-- { dg-final { scan-tree-dump-times ">>" 4 "optimized" } }
