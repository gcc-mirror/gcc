// { dg-do run  }
// Test that a throw in B's constructor destroys the A and frees the memory.
// Avoid use of none-overridable new/delete operators in shared
// { dg-options "-static" { target *-*-mingw* } }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

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
  newed = 0; // The libraries might call new before main starts.
  try {
    foo (new B (A ()));
  } catch (...) { }

  return !(!newed && !created);
}

A::A() { created = 1; }
A::~A() { created = 0; }
B::B(A) { throw 1; }
void foo (B*) { }

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
  free (p);
}

