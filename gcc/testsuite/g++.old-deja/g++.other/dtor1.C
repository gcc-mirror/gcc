// Test for use of typedef in explicit destructor call.

#include <new>

struct X {
  typedef X foo;
};

X x;
unsigned char bar[sizeof (X)];

int
main ()
{
  X* p = new (bar) X;
  p->~foo();
};
