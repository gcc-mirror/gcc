-- { dg-do compile }
-- { dg-options "-gnatws" }

with Uninit_Array_Pkg; use Uninit_Array_Pkg;

package body Uninit_Array is

  function F1 return Integer;
  pragma Inline_Always (F1);

  function F1 return Integer is
    Var : Arr;
  begin
    return F (Var(Var'First(1)));
  end;

  function F2 return Integer is
  begin
    return F1;
  end;

end Uninit_Array;
