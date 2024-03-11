/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include "tree-vect.h"

static int a[512], b[512];

void __attribute__((noinline,noclone))
foo (int *sum1p, int *sum2p, int *sum3p)
{
  int sum1 = 0;
  int sum2 = 0;
  int sum3 = 0;
  /* Check that we vectorize a reduction chain and a SLP reduction
     at the same time.  */
  for (int i = 0; i < 256; ++i)
    {
      sum1 += a[2*i];
      sum1 += a[2*i + 1];
      sum2 += b[2*i];
      sum3 += b[2*i + 1];
    }
  *sum1p = sum1;
  *sum2p = sum2;
  *sum3p = sum3;
}

int main()
{
  check_vect ();

  for (int i = 0; i < 256; ++i)
    {
      a[2*i] = i;
      a[2*i + 1] = i/2;
      b[2*i] = i + 1;
      b[2*i + 1] = i/2 + 1;
      __asm__ volatile ("" : : : "memory");
    }
  int sum1, sum2, sum3;
  foo (&sum1, &sum2, &sum3);
  if (sum1 != 48896 || sum2 != 32896 || sum3 != 16512)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
