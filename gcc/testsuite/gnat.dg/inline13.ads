with Inline13_Pkg;

package Inline13 is

  type Arr is array (Positive range <>) of Inline13_Pkg.T;

  function F (L : Arr) return String;

end Inline13;
