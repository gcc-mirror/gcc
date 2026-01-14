// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on namespaces.

constexpr auto glob = ^^::;

namespace Q { }
constexpr auto q = ^^Q;
namespace Q { }
static_assert (q == ^^Q);

namespace A { namespace B { using C = int; } }
constexpr auto r = ^^A::B::C &;

namespace N {
  using T = int;
  namespace M {
    using TT = int;
  }
}

void
f1 ()
{
  constexpr auto n = ^^N;
  [: n :]::T i = 42;
  typename [: n :]::T it = 42;

  constexpr auto t = ^^N::T;
  typename [: t :] j = 42;

  constexpr auto m = ^^N::M;
  [: m :]::TT k = 42;
  typename [: m :]::TT kt = 42;

  constexpr auto m2 = ^^t;
}
