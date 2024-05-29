/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test vectorizer can exploit ISA 2.07 instruction vmuluwm (Vector Multiply
   Unsigned Word Modulo) for both signed and unsigned word multiplication.  */

#define N 128

extern signed int si_a[N], si_b[N], si_c[N];
extern unsigned int ui_a[N], ui_b[N], ui_c[N];

__attribute__ ((noipa)) void
test_si ()
{
  for (int i = 0; i < N; i++)
    si_c[i] = si_a[i] * si_b[i];
}

__attribute__ ((noipa)) void
test_ui ()
{
  for (int i = 0; i < N; i++)
    ui_c[i] = ui_a[i] * ui_b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvmuluwm\M} 2 } } */
