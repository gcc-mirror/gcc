// Test that passing an object with non-trivial copy constructor and
// destructor is (conditionally) supported and has sensible semantics.

#include <stdarg.h>
extern "C" void abort();

void *as[5];
int i;

struct A {
  A() { as[i++] = this; }
  A(const A& a) {
    if (&a != as[i-1])
      abort();
    as[i++] = this;
  }
  ~A() {
    if (this != as[--i])
      abort();
  }
};

void f(int i, ...) {
  va_list ap;
  va_start (ap, i);
  A ar = va_arg (ap, A);
}

int main()
{
  f(42,A());
  if (i != 0)
    abort();
}
