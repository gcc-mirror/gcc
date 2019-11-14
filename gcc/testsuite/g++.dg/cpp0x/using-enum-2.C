// PR c++/60265
// { dg-do compile { target c++11 } }

namespace A
{
  enum class E { V };

  using E::V;        // { dg-error "name enumerator" }
}

void foo()
{
  using A::E::V;     // { dg-error "name enumerator" }
}

using A::E::V;       // { dg-error "name enumerator" }

enum class F { U };

using F::U;          // { dg-error "name enumerator" }
