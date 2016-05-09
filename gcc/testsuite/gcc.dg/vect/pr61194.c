/* { dg-require-effective-target vect_cond_mixed } */

#include "tree-vect.h"

static float x[1024];
static float y[1024];
static float z[1024];
static float w[1024];

void __attribute__((noinline,noclone)) barX()
{
  int i;
  for (i=0; i<1024; ++i)
    z[i] = ((x[i]>0) & (w[i]<0)) ? z[i] : y[i];
}

int main()
{
  int i;

  check_vect ();

  for (i = 0; i < 1024; ++i)
    {
      x[i] = -10 + i;
      w[i] = 100 - i;
      z[i] = 0.;
      y[i] = 1.;
      __asm__ volatile ("" : : : "memory");
    }

  barX();

  for (i = 0; i < 1024; ++i)
    if (z[i] != ((x[i]>0 && w[i]<0) ? 0. : 1.))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
