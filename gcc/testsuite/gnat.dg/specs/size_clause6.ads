-- { dg-do compile }

with Interfaces;

package Size_Clause6 is

  type Long_Double is new Interfaces.IEEE_Extended_Float;
  for Long_Double'Size use 128; -- { dg-warning "unused" "" { target { ! { { i?86-*-* x86_64-*-* } && lp64 } } } }

  function Int (X : in Long_Double) return Integer is (0);

end Size_Clause6;
