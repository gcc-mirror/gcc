/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define A 300
#define N 16

char src[N];
short dst[N];
short src1[N], dst1[N];

void foo (int a)
{
  dst[0] = (short) (a * (int) src[0]);
  dst[1] = (short) (a * (int) src[1]);
  dst[2] = (short) (a * (int) src[2]);
  dst[3] = (short) (a * (int) src[3]);
  dst[4] = (short) (a * (int) src[4]);
  dst[5] = (short) (a * (int) src[5]);
  dst[6] = (short) (a * (int) src[6]);
  dst[7] = (short) (a * (int) src[7]);
  dst[8] = (short) (a * (int) src[8]);
  dst[9] = (short) (a * (int) src[9]);
  dst[10] = (short) (a * (int) src[10]);
  dst[11] = (short) (a * (int) src[11]);
  dst[12] = (short) (a * (int) src[12]);
  dst[13] = (short) (a * (int) src[13]);
  dst[14] = (short) (a * (int) src[14]);
  dst[15] = (short) (a * (int) src[15]);

  dst1[0] += src1[0];
  dst1[1] += src1[1];
  dst1[2] += src1[2];
  dst1[3] += src1[3];
  dst1[4] += src1[4];
  dst1[5] += src1[5];
  dst1[6] += src1[6];
  dst1[7] += src1[7];
}


int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      dst[i] = 2;
      dst1[i] = 0;
      src[i] = i;
      src1[i] = i+2;
    }

  foo (A);

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (dst[i] != A * i
          || (i < N/2 && dst1[i] != i + 2))
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 2 "slp2" { target { vect_int_mult &&  { vect_pack_trunc && vect_unpack } } } } } */

