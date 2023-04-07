// PR c++/107532
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

#include <functional>

struct X { int n; };

struct S {
  std::reference_wrapper<const X> wrapit() const { return x; }
  X x;
};

void
g (const S& s)
{
  const auto& a1 = s.wrapit().get();
  (void) a1;
  const auto& a2 = S().wrapit().get(); // { dg-warning "dangling reference" }
  (void) a2;
}
