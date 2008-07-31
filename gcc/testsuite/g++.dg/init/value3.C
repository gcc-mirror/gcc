// Testcase for value-initialization in new-expressions.
// { dg-do run }

#include <stdlib.h>
#include <string.h>

// Make sure that we return memory that isn't already set to 0.
void *operator new(size_t s)
{
  void *p = malloc (s);
  memset (p, 42, s);
  return p;
}

struct A { A() {} ~A() {} };
struct B { A a; int i; };

int main()
{
  B *p = new B();
  if (p->i != 0)
    abort();

  p = new B[2]();
  if (p[0].i != 0 || p[1].i != 0)
    abort();

  B(*p2)[2] = new B[2][2]();
  if (p2[0][0].i != 0 || p2[0][1].i != 0)
    abort();
}
