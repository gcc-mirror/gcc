/* PR target/127649 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx" } */

/* LTGT is a signaling compare.  Use LT | GT for SSE and the NEQ_OS
   predicate for AVX.  */

void
ltgt_sse (int *r, float *a, float *b)
{
  for (int i = 0; i < 1024; i++)
    r[i] = (a[i] < b[i]) | (a[i] > b[i]);
}

__attribute__((target ("avx,no-avx512f")))
void
ltgt_avx (int *r, float *a, float *b)
{
  for (int i = 0; i < 1024; i++)
    r[i] = (a[i] < b[i]) | (a[i] > b[i]);
}

__attribute__((target ("avx512vl")))
void
ltgt_avx512 (int *r, float *a, float *b)
{
  for (int i = 0; i < 1024; i++)
    r[i] = (a[i] < b[i]) | (a[i] > b[i]);
}

/* { dg-final { scan-assembler-times "\[ \t\]cmpltps" 2 } } */
/* { dg-final { scan-assembler-not "\[ \t\]cmpordps" } } */
/* { dg-final { scan-assembler-not "\[ \t\]cmpneqps" } } */
/* { dg-final { scan-assembler-times "vcmpneq_osps" 1 } } */
/* { dg-final { scan-assembler-times "vcmpps\[ \t\]*\\\$28," 1 } } */
/* { dg-final { scan-assembler-not "vcmpneq_oqps" } } */
/* { dg-final { scan-assembler-not "vcmpps\[ \t\]*\\\$12," } } */
