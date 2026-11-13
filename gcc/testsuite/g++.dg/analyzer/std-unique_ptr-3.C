// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <memory>

struct A {int x; int y;};

int
test_use_after_move (std::unique_ptr<A> p)
{
  std::unique_ptr<A> q = std::move (p);

  if (p->x > 10) // { dg-warning "dereference of NULL" }
    return 1;

  return q->x * q->y;
}
