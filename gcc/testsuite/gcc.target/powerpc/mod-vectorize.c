/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test vectorizer can exploit ISA 3.1 instructions Vector Modulo
   Signed/Unsigned Word/Doubleword for word/doubleword modulo operations.  */

#define N 128

extern signed int si_a[N], si_b[N], si_c[N];
extern unsigned int ui_a[N], ui_b[N], ui_c[N];
extern signed long long sd_a[N], sd_b[N], sd_c[N];
extern unsigned long long ud_a[N], ud_b[N], ud_c[N];

__attribute__ ((noipa)) void
test_si ()
{
  for (int i = 0; i < N; i++)
    si_c[i] = si_a[i] % si_b[i];
}

__attribute__ ((noipa)) void
test_ui ()
{
  for (int i = 0; i < N; i++)
    ui_c[i] = ui_a[i] % ui_b[i];
}

__attribute__ ((noipa)) void
test_sd ()
{
  for (int i = 0; i < N; i++)
    sd_c[i] = sd_a[i] % sd_b[i];
}

__attribute__ ((noipa)) void
test_ud ()
{
  for (int i = 0; i < N; i++)
    ud_c[i] = ud_a[i] % ud_b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
/* { dg-final { scan-assembler-times {\mvmodsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmoduw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodud\M} 1 } } */
