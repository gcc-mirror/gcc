#include <stdbool.h>
#include "tree-vect.h"

struct C
{
    int c;
    int d;
    bool f :1;
    float e;
};

struct A
{
  unsigned int a;
  unsigned char c1, c2;
  bool b1 : 1;
  bool b2 : 1;
  bool b3 : 1;
  struct C b4;
};

void __attribute__((noipa))
foo (const struct A * __restrict x, int y)
{
  int s = 0, i = 0;
  for (i = 0; i < y; ++i)
    {
      const struct A a = x[i];
      s += a.b4.f ? 1 : 0;
    }
  if (s != 0)
    __builtin_abort ();
}

int
main ()
{
  struct A x[100];
  int i;

  check_vect ();

  __builtin_memset (x, -1, sizeof (x));
#pragma GCC novect
  for (i = 0; i < 100; i++)
    {
      x[i].b1 = false;
      x[i].b2 = false;
      x[i].b3 = false;
      x[i].b4.f = false;
    }
  foo (x, 100);
  return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target vect_int } } } */
