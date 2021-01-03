// PR c++/60265
// { dg-do compile { target c++11 } }

// [namespace.udecl]/7 shall not name a scoped enumerator.
// (this changes in C++2a)

namespace A
{
  enum class E { V };

  using E::V;        // { dg-error "enum" "" { target { ! c++2a } } }
}

void foo()
{
  using A::E::V;     // { dg-error "enum" "" { target { ! c++2a } } }
}

using A::E::V;       // { dg-error "enum" "" { target { ! c++2a } } }

enum class F { U };

using F::U;          // { dg-error "enum" "" { target { ! c++2a } } }
