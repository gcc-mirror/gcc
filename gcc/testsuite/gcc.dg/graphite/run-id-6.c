/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -fno-tree-scev-cprop -fgraphite-identity" } */
/* { dg-require-effective-target vect_int } */

/* gcc.dg/vect/no-scevccp-outer-4.c was miscompiled by Graphite.
   Adding it here to always test it with Graphite.  */

#include <stdarg.h>

extern void abort ();
#define N 40

int a[N];

/* induction variable k advances through inner and outer loops.  */

__attribute__ ((noinline)) int
foo (int n){
  int i,j,k=0;
  int sum;

  if (n<=0)
    return 0;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < n; j+=2) {
      sum += k++;
    }
    a[i] = sum + j;
  }
}

int main (void)
{
  int i,j,k=0;
  int sum;

  for (i=0; i<N; i++)
    a[i] = i;

  foo (N);

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j+=2)
        sum += k++;
      if (a[i] != sum + j)
	abort();
    }

  return 0;
}

