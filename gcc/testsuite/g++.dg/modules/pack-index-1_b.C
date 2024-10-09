// { dg-additional-options { -std=c++26 -fmodules-ts } }

import packing1;

int
main ()
{
  using U = Type<1, char, int, float>;
  using U = int;

  U r = foo<2, 0, 1, 42>();

  constexpr auto V = Var<2, 0, 1, 42>;
  static_assert (V == 42);
}
