// { dg-do run  }
// Test for calling placement delete.

#include <new>
#include <stddef.h>

int r = 1;

struct A {
  A() { throw 1; }
  void operator delete (void *p, int, int) { r = 0; ::operator delete (p); }
};

void * operator new (size_t size, int, int) { return operator new (size); }

int main ()
{
  try {
    A* ap = new (1, 5) A;
  } catch (...) {  }

  return r;
}
