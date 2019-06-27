// DR 1940 - static_assert in anonymous unions
// { dg-do compile { target c++11 } }

namespace N {
  static union { int i; static_assert(1, ""); };
}

void
g ()
{
  union { int j; static_assert(1, ""); };
  N::i = 42;
}
