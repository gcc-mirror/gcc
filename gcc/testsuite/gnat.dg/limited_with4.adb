-- { dg-do compile }

with Limited_With4_Pkg;

package body Limited_With4 is

  procedure Proc1 (A : Limited_With4_Pkg.Rec12 ; I : Integer) is
  begin
    if A.R.I /= I then
      raise Program_Error;
    end if;
  end;

  function Func1 (I : Integer) return Limited_With4_Pkg.Rec12 is
  begin
    return (I => I, R => (I => I));
  end;

  procedure Proc2 (A : Limited_With4_Pkg.Rec22 ; I : Integer) is
  begin
    if A.R.I /= I then
      raise Program_Error;
    end if;
  end;

  function Func2 (I : Integer) return Limited_With4_Pkg.Rec22 is
  begin
    return (I => I, R => (I => I));
  end;

  procedure Proc3 (A : Limited_With4_Pkg.Rec12 ; B : Limited_With4_Pkg.Rec22) is
  begin
    if A.R.I /= B.R.I then
      raise Program_Error;
    end if;
  end;

  function Func3 (A : Limited_With4_Pkg.Rec12) return Limited_With4_Pkg.Rec22 is
  begin
    return (I => A.R.I, R => (I => A.R.I));
  end;

end Limited_With4;
