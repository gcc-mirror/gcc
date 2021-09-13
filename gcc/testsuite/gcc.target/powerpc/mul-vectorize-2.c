/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test vectorizer can exploit ISA 3.1 instruction vmulld (Vector Multiply
   Low Doubleword) for both signed and unsigned doubleword multiplication.  */

#define N 128

extern signed long long sd_a[N], sd_b[N], sd_c[N];
extern unsigned long long ud_a[N], ud_b[N], ud_c[N];

__attribute__ ((noipa)) void
test_sd ()
{
  for (int i = 0; i < N; i++)
    sd_c[i] = sd_a[i] * sd_b[i];
}

__attribute__ ((noipa)) void
test_ud ()
{
  for (int i = 0; i < N; i++)
    ud_c[i] = ud_a[i] * ud_b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvmulld\M} 2 } } */
