-- { dg-do compile }

package body Aggr22 is

  type Ptr is access all Integer;
  type Arr is array (Positive range <>) of Ptr;

  procedure Proc is
    A : Arr (1 .. 33);
  begin
    A := (1 => null, 2 .. 32 => My_Rec.I'Access, 33 => null);
  end;

end Aggr22;
