// PR c++/60265
// { dg-do compile { target c++11 } }

namespace A
{
  enum class E { V };

  using E::V;        // { dg-error "not a namespace" }
}

void foo()
{
  using A::E::V;     // { dg-error "not a namespace" }
}

using A::E::V;       // { dg-error "not a namespace" }

enum class F { U };

using F::U;          // { dg-error "not a namespace" }
