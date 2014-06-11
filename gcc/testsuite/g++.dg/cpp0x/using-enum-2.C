// PR c++/60265
// { dg-do compile { target c++11 } }

namespace A
{
  enum class E { V };

  using E::V;        // { dg-error "not a namespace or unscoped enum" }
}

void foo()
{
  using A::E::V;     // { dg-error "not a namespace or unscoped enum" }
}

using A::E::V;       // { dg-error "not a namespace or unscoped enum" }

enum class F { U };

using F::U;          // { dg-error "not a namespace or unscoped enum" }
