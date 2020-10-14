/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define NINTS (VECTOR_BITS / 32)
#else
#define NINTS 8
#endif

#define N (NINTS + 1)

struct extraction
{
  int a[N];
  int b[N];
};

static int a[N] = {1,2,3,4,5,6,7,8,9};
static int b[N] = {17,24,7,0,2,3,4,31,82};

__attribute__ ((noinline))
int main1 (int x, int y) {
  int i;
  struct extraction *p;
  p = (struct extraction *) malloc (sizeof (struct extraction));

  for (i = 0; i < N; i++)
    {
      p->a[i] = a[i];
      p->b[i] = b[i];
      asm volatile ("" ::: "memory");
    }

  /* Vectorizable: distance > VF.  */
  for (i = 0; i < N; i++)
    *((int *)p + x + i) = *((int *)p + x + i + NINTS);

  /* check results: */
  if (p->a[0] != a[N - 1])
    abort ();
  for (i = 1; i < N; i++)
    if (p->a[i] != b[i - 1])
      abort ();

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (0, N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
