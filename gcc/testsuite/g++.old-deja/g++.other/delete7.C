// { dg-do run  }
// Test that we call a class-specific vector op delete.

#include <new>

int r = 1;

struct A
{
  void operator delete[](void *p) { r = 0; ::operator delete (p); }
};

int main ()
{
  A (*p)[2] = new A[2][2];
  delete [] p;
  return r;
}
