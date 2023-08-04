/* { dg-require-effective-target vect_int } */
/* We now if-convert the loop unconditonally as the memory locations
   are always stored to.  */
/* { dg-additional-options "-fno-tree-loop-if-convert" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 50

int a[N], b[N], in1[N], in2[N];
int result[2*N] = {5,-7,7,-6,9,-5,11,-4,13,-3,15,-2,17,-1,19,0,21,1,23,2,25,3,27,4,29,5,31,6,33,7,35,8,37,9,39,10,41,11,43,12,45,13,47,14,49,15,51,16,53,17,55,18,57,19,59,20,61,21,63,22,65,23,67,24,69,25,71,26,73,27,75,28,77,29,79,30,81,31,83,32,85,33,87,34,89,35,91,36,93,37,95,38,97,39,99,40,101,41,103,42};

__attribute__ ((noinline)) void
foo (int *pa, int *pb)
{
  int i;
  int c, d;

  /* Store sinking should not work here since the pointers may alias.  */
  for (i = 0; i < N; i++)
    {
      c = in1[i];
      d = in2[i];

      if (c >= d)
        {
          *pa = c;
          *pb = d + 5;
        }
      else
        {
          *pb = d - 12;
          *pa = c + d;
        }

      pa++;
      pb++;
    }
}

int
main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in1[i] = i;
      in2[i] = i + 5;
      __asm__ volatile ("");
    }

  foo (a, b);

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (a[i] != result[2*i] || b[i] != result[2*i+1])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect"  } } */
