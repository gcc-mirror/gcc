/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int a[N], b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* Vectorization of reduction. Loop-aware SLP is not possible, because of 
   different arrays.  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1)
{
  int i;
  int sum0 = 0, sum1 = 0;

  for (i = 0; i < n; i++) {
    sum1 += a[2*i];
    sum0 += b[2*i];
  }

  /* Check results:  */
  if (sum0 != res0
      || sum1 != res1)
    abort ();

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    a[i] = b[i] = i;

  main1 (N/2, 4032, 4032);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail { vect_no_int_add || { ! vect_unpack } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "different interleaving chains in one node" 1 "vect" { target { ! vect_no_int_add } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

