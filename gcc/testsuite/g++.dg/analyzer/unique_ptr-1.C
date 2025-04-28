// Verify that we complain about trivial uses of NULL unique_ptr.

// { dg-do compile { target c++11 } }

#include <memory>

struct A {int x; int y;};

int main() {
  std::unique_ptr<A> a;
  a->x = 12; // { dg-warning "dereference of NULL" }
  return 0;
}
