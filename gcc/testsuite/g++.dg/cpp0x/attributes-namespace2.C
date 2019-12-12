// PR c++/79817 - attribute deprecated on namespace.
// { dg-do compile { target c++11 } }

namespace [[deprecated]] { // { dg-warning "ignoring .deprecated. attribute on anonymous namespace" }
  int nn;
}

inline namespace [[deprecated]] I { 
  int x;
}

namespace M {
  int y;
  inline namespace [[deprecated]] N {
    int x;
  }
}

void
g ()
{
  nn = 42;
  I::x = 42; // { dg-warning ".I. is deprecated" }
  M::x = 42;
  M::y = 42;
  M::N::x = 42; // { dg-warning ".M::N. is deprecated" }
}
