-- { dg-excess-errors "no code generated" }

package Opt7_Pkg is

  type Enum is (A, B);

  function Image (E : Enum) return String with Inline;

end Opt7_Pkg;
