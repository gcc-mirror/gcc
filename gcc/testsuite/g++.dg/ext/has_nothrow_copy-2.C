// PR c++/36871
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

struct A {
  template <class T> A (T)
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
  ;
};
struct B {
  B (B&) throw ();
  template <class T> B (T)
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
  ;
};

int main ()
{
  assert (__has_nothrow_copy (A));
  assert (__has_nothrow_copy (B));
}
