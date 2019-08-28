// PR c++/79817 - attribute deprecated on namespace.
// { dg-do compile { target c++11 } }

namespace [[deprecated]] N
{
  typedef decltype(sizeof(int)) T;
  int x;

  namespace N2 {
    typedef decltype(sizeof(int)) T;
    int y;
  }
}

namespace M {
  namespace [[deprecated]] M2 {
    typedef decltype(sizeof(int)) T;
    int z;
  }
}

void
fn2 ()
{
  using N::x; // { dg-warning ".N. is deprecated" }
  N::T j; // { dg-warning ".N. is deprecated" }

  using M::M2::z; // { dg-warning ".M::M2. is deprecated" }
  M::M2::T l; // { dg-warning ".M::M2. is deprecated" }

  using N::N2::y; // { dg-warning ".N. is deprecated" }
  N::N2::T k; // { dg-warning ".N. is deprecated" }
}
