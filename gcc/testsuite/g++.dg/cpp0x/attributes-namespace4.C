// PR c++/79817 - attribute deprecated on namespace.
// { dg-do compile { target c++11 } }

namespace [[deprecated]] N {
  struct S { };
  using T = int;
  const int value = 42;
  int arr[10];
}

namespace [[deprecated]] Y {
  int x;
  int i = x;
}

namespace [[deprecated]] M {
  namespace M2 {
  }
}

enum E { F =  N::value }; // { dg-warning ".N. is deprecated" }

template<N::T> // { dg-warning ".N. is deprecated" }
struct X { };

N::T foo(); // { dg-warning ".N. is deprecated" }

void
g(N::T p) // { dg-warning ".N. is deprecated" }
{
  N::S s; // { dg-warning ".N. is deprecated" }
  N::arr[0] = 42; // { dg-warning ".N. is deprecated" }
}

namespace Z = Y; // { dg-warning ".Y. is deprecated" }
namespace Z2 = M::M2; // { dg-warning ".M. is deprecated" }

void
g2 ()
{
  using namespace Y; // { dg-warning ".Y. is deprecated" }
  using namespace M::M2; // { dg-warning ".M. is deprecated" }
  using TT = N::T; // { dg-warning ".N. is deprecated" }
  using N::T; // { dg-warning ".N. is deprecated" }
}
