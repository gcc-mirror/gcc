/* { dg-do compile } */

#include <stdlib.h>

#define N 128

int a[N];

int main1 (int res0, int res1)
{
  int i;
  int sum0 = 0, sum1 = 0;

  for (i = 0; i < N/2; i++) {
    sum1 += a[2*i];
    sum0 += a[2*i];
  }

  /* Check results:  */
  if (sum0 != res0
      || sum1 != res1)
    abort ();

  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
