/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

#include <stdlib.h>

#define N 3200

extern void abort (void);
typedef unsigned short T;

__attribute__ ((noinline)) void
testmax (const T *c, T init, T result)
{
  T lc[N], accum = init;
  int i;

  __builtin_memcpy (lc, c, sizeof(lc));

  for (i = 0; i < N; i++) {
    accum = accum < lc[i] ? lc[i] : accum;
  }

  if (accum != result)
    abort ();
}

__attribute__ ((noinline)) void
testmin (const T *c, T init, T result)
{
  T lc[N], accum = init;
  int i;

  __builtin_memcpy (lc, c, sizeof(lc));

  for (i = 0; i < N; i++) {
    accum = accum > lc[i] ? lc[i] : accum;
  }

  if (accum != result)
    abort ();
}

int main (void)
{ 
  static unsigned short A[N] = {
    0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008,
    0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 0x0010,
    0x7000, 0x7100, 0x7200, 0x7300, 0x7400, 0x7500, 0x7600, 0x7700,
    0x7ff8, 0x7ff9, 0x7ffa, 0x7ffb, 0x7ffc, 0x7ffd, 0x7ffe, 0x7fff
  };

  static unsigned short B[N] = {
    0x7000, 0x7100, 0x7200, 0x7300, 0x7400, 0x7500, 0x7600, 0x7700,
    0x7ff8, 0x7ff9, 0x7ffa, 0x7ffb, 0x7ffc, 0x7ffd, 0x7ffe, 0x7fff,
    0x8000, 0x8001, 0x8002, 0x8003, 0x8004, 0x8005, 0x8006, 0x8007,
    0x8008, 0x8009, 0x800a, 0x800b, 0x800c, 0x800d, 0x800e, 0x800f
  };

  static unsigned short C[N] = {
    0xffff, 0xfffe, 0xfffd, 0xfffc, 0xfffb, 0xfffa, 0xfff9, 0xfff8,
    0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 0x0010,
    0x8000, 0x8001, 0x8002, 0x8003, 0x8004, 0x8005, 0x8006, 0x8007,
    0x7000, 0x7100, 0x7200, 0x7300, 0x7400, 0x7500, 0x7600, 0x7700,
  };

  int i;

  for (i=32; i<N; i++)
    {
      A[i]= 0x0001;
      B[i]= 0x7000;
      C[i]= 0xffff;
    }

  testmin (A, 10, 1);
  testmin (B, 0x7fff, 0x7000);
  testmin (C, 0x7fff, 0x0009);

  testmax (A, 0, 0x7fff);
  testmax (B, 0, 0x800f);
  testmax (C, 0, 0xffff);

  return 0;
}


/* { dg-final { scan-tree-dump-times "Detected reduction" 2 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 3 "parloops2" } } */
