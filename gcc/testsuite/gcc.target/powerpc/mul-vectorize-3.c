/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test vectorizer can exploit ISA 3.1 instructions Vector Multiply
   High Signed/Unsigned Word for both signed and unsigned int high part
   multiplication.  */

#define N 128

extern signed int si_a[N], si_b[N], si_c[N];
extern unsigned int ui_a[N], ui_b[N], ui_c[N];

typedef signed long long sLL;
typedef unsigned long long uLL;

__attribute__ ((noipa)) void
test_si ()
{
  for (int i = 0; i < N; i++)
    si_c[i] = ((sLL) si_a[i] * (sLL) si_b[i]) >> 32;
}

__attribute__ ((noipa)) void
test_ui ()
{
  for (int i = 0; i < N; i++)
    ui_c[i] = ((uLL) ui_a[i] * (uLL) ui_b[i]) >> 32;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvmulhsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhuw\M} 1 } } */
