package body Lto13_Pkg is

  procedure Proc is
  begin
    raise Constraint_Error;
  end;

  type T is null record;

end Lto13_Pkg;
