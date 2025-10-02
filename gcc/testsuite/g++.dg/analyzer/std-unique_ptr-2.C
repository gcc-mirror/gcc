// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <memory>

struct A {int x; int y;};

extern std::unique_ptr<A> make_ptr ();

int test (int flag) {
  std::unique_ptr<A> a;
  if (flag)
    a = make_ptr ();
  a->x = 12; // { dg-warning "dereference of NULL" "" { xfail *-*-*} }
  // TODO: this is failing due to "too complex" warnings
  return 0;
}
