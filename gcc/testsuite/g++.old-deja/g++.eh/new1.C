// Test that a throw in foo destroys the A, but does not free the memory.

#include <stddef.h>
#include <stdlib.h>
#include <new.h>

struct A {
  A();
  ~A();
};

struct B {
  B (A);
};

void foo (B*);

int newed, created;

int main ()
{
  try {
    foo (new B (A ()));
  } catch (...) { }

  return !(newed && !created);
}

A::A() { created = 1; }
A::~A() { created = 0; }
B::B(A) { }
void foo (B*) { throw 1; }

void* operator new (size_t size) throw (std::bad_alloc)
{
  ++newed;
  return (void *) malloc (size);
}

void operator delete (void *p) throw ()
{
  --newed;
  free (p);
}
