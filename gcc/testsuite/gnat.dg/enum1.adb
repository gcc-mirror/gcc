-- { dg-do run }
-- { dg-options "-O2" }

with Enum1_Pkg; use Enum1_Pkg;

procedure Enum1 is

  function Cond return Boolean is
  begin
    return My_N = Two or My_N = Three;
  end;

begin
  if Cond then
    raise Constraint_Error;
  end if;
end;
