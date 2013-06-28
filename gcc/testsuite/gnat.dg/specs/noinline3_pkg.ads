-- { dg-excess-errors "cannot generate code" }

generic

  I : Integer;

package Noinline3_Pkg is

  function F (A, B : Integer) return Integer;

end Noinline3_Pkg;
