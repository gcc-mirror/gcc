// PR c++/28407
// A declaration in the anonymous namespace still has external linkage.

template <int *P> class A { };
namespace
{
  int i;
}

A<&i> a;
