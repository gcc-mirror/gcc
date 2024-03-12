/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int x[1024], y[1024];

void __attribute__((noipa)) foo()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[1023 - (2*i)];
      x[2*i+1] = y[1023 - (2*i+1)];
    }
}

void __attribute__((noipa)) bar()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = y[1023 - (2*i+1)];
      x[2*i+1] = y[1023 - (2*i)];
    }
}

int 
main ()
{
  check_vect ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      y[i] = i;
      __asm__ volatile ("");
    }

  foo ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[1023 - i])
      abort ();

  for (int i = 0; i < 1024; ++i)
    {
      x[i] = 0;
      __asm__ volatile ("");
    }

  bar ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    if (x[i] != y[1023 - i^1])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" } } */
