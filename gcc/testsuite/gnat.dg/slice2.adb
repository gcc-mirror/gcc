-- { dg-do compile }
-- { dg-options "-O" }

package body Slice2 is

  function F (I : R1) return R2 is
    Val : R2;
  begin
    Val.Text (1 .. 8) := I.Text (1 .. 8);
    return Val;
  end F;

end Slice2;
