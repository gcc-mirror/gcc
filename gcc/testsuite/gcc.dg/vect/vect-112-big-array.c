/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

char cb[N];
char cc[N];

__attribute__ ((noinline)) int
main1 (void)
{
  int i;
  int diff = 0;
  int check_diff = 0;
  for (i = 0; i < N; i++) {
    cb[i] = i + 2;
    cc[i] = i + 1;
    check_diff += (cb[i] - cc[i]);
    asm volatile ("" ::: "memory");
  }

  /* Cross-iteration cycle.  */
  diff = 0;
  for (i = 0; i < N; i++) {
    diff += (cb[i] - cc[i]);
  }

  /* Check results.  */
  if (diff != check_diff)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_unpack } } } */


/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
