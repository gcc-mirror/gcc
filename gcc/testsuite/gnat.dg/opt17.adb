-- { dg-do compile }
-- { dg-options "-O" }

package body Opt17 is

  function Func return S is
    V : String (1 .. 6);
  begin
    V (1 .. 3) := "ABC";
    return V (1 .. 5);
  end;

end Opt17;
