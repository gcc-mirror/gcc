/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int
main1 (void)
{
  int i;
  float a[N];

  /* Induction.  */
  for ( i = 0; i < N; i++) 
  {
    a[i] = i;
  }

  for ( i = 0; i < N; i++) 
  {
    if (a[i] != i)
	abort ();
  }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
