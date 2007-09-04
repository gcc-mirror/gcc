/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

float pa[N] __attribute__ ((__aligned__(16)));
float pb[N] __attribute__ ((__aligned__(16))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
float pc[N] __attribute__ ((__aligned__(16))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

/* Check handling of unaligned accesses when the misalignment is
   known at compile time and different accesses have the same
   misalignment (e.g. peeling to align one access will align all
   accesses with the same misalignment.  Also, the number of 
   peeled iterations is known in this case, and the vectorizer
   can use this information (generate prolog and epilog loops
   with known number of iterations, and only if needed).  */

__attribute__ ((noinline)) int
main1 ()
{
  int i;

  for (i = 0; i < 5; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < 5; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

__attribute__ ((noinline)) int
main2 ()
{
  int i;

  for (i = 0; i < 6; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < 6; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

__attribute__ ((noinline)) int
main3 (int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  main1 ();
  main2 ();
  main3 (N-1);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 3 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
