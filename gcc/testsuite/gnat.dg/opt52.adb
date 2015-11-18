-- { dg-do compile }
-- { dg-options "-O -fdump-tree-optimized" }

procedure Opt52 (I : Integer) is
begin
  if I + 1 < I then
    raise Program_Error;
  end if;
end;

-- { dg-final { scan-tree-dump-not "check_PE_Explicit_Raise" "optimized" } }
