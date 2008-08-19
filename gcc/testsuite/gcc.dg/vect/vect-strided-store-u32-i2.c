/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 16

int a[N*2];
int b[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
int c[N] = {1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31};

__attribute__ ((noinline)) int
main1 (void)
{
  int i;

  /* Strided access pattern.  */
  for (i = 0; i < N/2; i++)
    {
      a[i*2] = b[i] + c[i];
      a[i*2+1] = b[i] * c[i];
    }

  /* Check results.  */
  for (i = 0; i < N/2; i++)
    {
      if (a[i*2] != b[i] + c[i]
          || a[i*2+1] != b[i] * c[i])
        abort();
    }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* Needs interleaving support.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_interleave } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { xfail { vect_interleave } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

