// PR c++/36871
// { dg-do run }
#include <cassert>

struct A {
  template <class T> A (T)
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
  ;
};
struct B {
  B (B&) throw ();
  template <class T> B (T)
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
  ;
};

int main ()
{
  assert (__has_nothrow_copy (A));
  assert (__has_nothrow_copy (B));
}
