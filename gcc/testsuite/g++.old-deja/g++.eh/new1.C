// { dg-do run  }
// Test that a throw in foo destroys the A, but does not free the memory.
// Avoid use of none-overridable new/delete operators in shared
// { dg-options "-static" { target *-*-mingw* } }

#include <cstddef>
#include <cstdlib>
#include <new>

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

void* operator new (size_t size)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  ++newed;
  return (void *) std::malloc (size);
}

void operator delete (void *p) throw ()
{
  --newed;
  std::free (p);
}




