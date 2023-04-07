/* { dg-do compile { target { lp64 && s390_vx } } } */
/* { dg-options "-mzarch -march=native -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops" } */

/* { dg-additional-options "--param=min-vect-loop-bound=0 --param=vect-partial-vector-usage=2 -fno-trapping-math" } */

#define SZ 333

void foo (char *restrict a, char *restrict b, char *restrict c, int n)
{
#pragma gcc unroll 0
    for (int i = 0; i < 17; i++)
          c[i] = a[i] + b[i];
/* { dg-final { scan-assembler-times "lhi\t%r\[0-9\]*,0\n" 1 } } */
}

