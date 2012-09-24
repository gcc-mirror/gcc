-- { dg-do compile }

package body Atomic5 is

  function Create return R is
  begin
    return (A => 0, B => 1, C => 2, D => 4);
  end;

  procedure Proc1 is
    I : Unsigned_32;
  begin
    I := Conv(Create);
  end;

  procedure Proc2 is
    I : Unsigned_32;
  begin
    I := Conv(R'(A => 0, B => 1, C => 2, D => 4));
  end;

end Atomic5;
