/* PR target/127114 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -fno-trapping-math" } */

void
round_hf_narrow (_Float16 *__restrict a, _Float16 *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = (_Float16) __builtin_round (b[i]);
}

/* { dg-final { scan-assembler "vrndscaleph" } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*round" } } */
/* { dg-final { scan-assembler-not "vcvtsh2sd" } } */
