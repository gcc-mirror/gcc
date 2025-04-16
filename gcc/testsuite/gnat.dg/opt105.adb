-- { dg-do run }
-- { dg-options "-O" }

with Opt105_Pkg; use Opt105_Pkg;

procedure Opt105 is

  Val : constant Enum :=
          (if Enabled then (if Disabled then Two else One) else Three);

begin
  if Cond1 then
    return;
  end if;

  if Cond2 then
    return;
  end if;

  case Val is
    when One =>
      raise Program_Error;

    when Two =>
      raise Constraint_Error;

    when Three =>
      null;
  end case;
end;
