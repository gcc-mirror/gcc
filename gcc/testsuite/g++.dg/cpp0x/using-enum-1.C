// PR c++/60265
// { dg-do compile { target c++11 } }

namespace A
{
  enum E { V };

  using E::V;
}

void foo()
{
  using A::E::V;
}

using A::E::V;

enum F { U };

using F::U;
