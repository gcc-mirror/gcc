/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 50

typedef struct {
  short a;
  short b;
} data;

data in1[N], in2[N], out[N];
short result[N*2] = {7,-7,9,-6,11,-5,13,-4,15,-3,17,-2,19,-1,21,0,23,1,25,2,27,3,29,4,31,5,33,6,35,7,37,8,39,9,41,10,43,11,45,12,47,13,49,14,51,15,53,16,55,17,57,18,59,19,61,20,63,21,65,22,67,23,69,24,71,25,73,26,75,27,77,28,79,29,81,30,83,31,85,32,87,33,89,34,91,35,93,36,95,37,97,38,99,39,101,40,103,41,105,42};
short out1[N], out2[N];

__attribute__ ((noinline)) void
foo ()
{
  int i;
  short c, d;

  /* Vectorizable with conditional store sinking.  */
  for (i = 0; i < N; i++)
    {
      c = in1[i].b;
      d = in2[i].b;

      if (c >= d)
        {
          out[i].b = c;
          out[i].a = d + 5;
        }
      else
        {
          out[i].b = d - 12;
          out[i].a = c + d;
        }
    }

  /* Not vectorizable.  */
  for (i = 0; i < N; i++)
    {
      c = in1[i].b;
      d = in2[i].b;

      if (c >= d)
        {
          out1[i] = c;
        }
      else
        {
          out2[i] = c + d;
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

  for (i = 0; i < N; i++)
    {
      if (out[i].a != result[2*i] || out[i].b != result[2*i+1])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { xfail { vect_no_align || {! vect_strided } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
