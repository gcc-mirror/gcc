/* { dg-do run } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */
/* { dg-additional-options "-fno-assume-sane-operators-new-delete" } */

#include <stdlib.h>
#include <assert.h>

static bool flag = false;

class C
{
  bool prev;

public:
  C() : prev(flag)
  {
    flag = true;
  }

  ~C() {
    flag = prev;
  }
};

void* operator new(size_t size)
{
  assert(flag);
  return malloc(size);
}

void operator delete(void *p)
{
  free(p);
}

void g(int* p)
{
  delete p;
}

void f()
{
  int* p;
  {
    C c;
    p = new int;
  }
  g(p);
}

int main(int, char**)
{
  f();
}
