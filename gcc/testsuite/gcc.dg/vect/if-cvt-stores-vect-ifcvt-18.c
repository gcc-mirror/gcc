/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 50

typedef struct {
  short a;
  short b;
} data;

data in1[N], in2[N], out[N];
short result[N*2] = {10,-7,11,-6,12,-5,13,-4,14,-3,15,-2,16,-1,17,0,18,1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36,19,37,20,38,21,39,22,40,23,41,24,42,25,43,26,44,27,45,28,46,29,47,30,48,31,49,32,50,33,51,34,52,35,53,36,54,37,55,38,56,39,57,40,58,41,59,42};
short out1[N], out2[N];

__attribute__ ((noinline)) void
foo ()
{
  int i;
  short c, d;

  for (i = 0; i < N; i++)
    {
      c = in1[i].b;
      d = in2[i].b;

      if (c >= d)
        {
          out[i].b = in1[i].a;
          out[i].a = d + 5;
        }
      else
        {
          out[i].b = d - 12;
          out[i].a = in2[i].a + d;
        }
    }
}

int
main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in1[i].a = i;
      in1[i].b = i + 2;
      in2[i].a = 5;
      in2[i].b = i + 5;
      __asm__ volatile ("");
    }

  foo ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i].a != result[2*i] || out[i].b != result[2*i+1])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { xfail { { vect_no_align && { ! vect_hw_misalign } } || { ! vect_strided2 } } } } } */
