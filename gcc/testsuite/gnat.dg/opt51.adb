-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with Opt51_Pkg; use Opt51_Pkg;

procedure Opt51 (E: Enum; F : out Float) is
begin
  case (E) is
    when One =>
      F := 1.0;
    when Two =>
      F := 2.0;
    when Three =>
      F := 3.0;
    when others =>
      raise Program_Error;
  end case;
end;

-- { dg-final { scan-tree-dump-not "check_PE_Explicit_Raise" "optimized" } }
