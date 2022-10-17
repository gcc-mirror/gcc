/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test vectorizer can exploit ISA 3.1 instructions Vector Multiply
   High Signed/Unsigned Doubleword for both signed and unsigned long
   long high part multiplication.  */

#define N 128

extern signed long long sll_a[N], sll_b[N], sll_c[N];
extern unsigned long long ull_a[N], ull_b[N], ull_c[N];

typedef signed __int128 s128;
typedef unsigned __int128 u128;

__attribute__ ((noipa)) void
test_sll ()
{
  for (int i = 0; i < N; i++)
    sll_c[i] = ((s128) sll_a[i] * (s128) sll_b[i]) >> 64;
}

__attribute__ ((noipa)) void
test_ull ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = ((u128) ull_a[i] * (u128) ull_b[i]) >> 64;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvmulhsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhud\M} 1 } } */
