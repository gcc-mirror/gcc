-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with Ada.Command_Line; use Ada.Command_Line;
with Opt86_Pkg; use Opt86_Pkg;

procedure Opt86c is

  S1, S2, S3, S4 : Enum;

begin

  S1 := Enum'Value (Argument (1));
  S2 := Enum'Value (Argument (2));
  S3 := Enum'Value (Argument (3));
  S4 := Enum'Value (Argument (4));

  if S1 in Val16 | Val8 | Val26 | Val2 | Val10 then
    raise Program_Error;
  end if;

  if S2 not in Val16 | Val8 | Val26 | Val2 | Val10 then
    raise Program_Error;
  end if;

  if S3 = Val3 or S3 = Val25 or S3 = Val13 or S3 = Val29 or S3 = Val11 then
    raise Program_Error;
  end if;

  if S4 /= Val3 and S4 /= Val25 and S4 /= Val13 and S4 /= Val29 and s4 /= Val11 then
    raise Program_Error;
  end if;

end;

-- { dg-final { scan-tree-dump-not "> 26" "optimized" } }
-- { dg-final { scan-tree-dump-not "> 29" "optimized" } }
