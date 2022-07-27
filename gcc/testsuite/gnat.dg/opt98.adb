-- { dg-do compile }
-- { dg-options "-O -gnatws" }

package body Opt98 is

  function Func return Rec is
    R :Rec;
  begin
    A := To_Address ((I => 0));
    R := To_Rec (A);
    return R;
  end;

end Opt98;
