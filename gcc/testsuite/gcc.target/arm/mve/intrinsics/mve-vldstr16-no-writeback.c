/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

void
fn1 (__fp16 *pSrc)
{
  __fp16 high;
  __fp16 *pDst = 0;
  unsigned i;
  for (i = 0;; i++)
    if (pSrc[i])
      pDst[i] = high;
}

/* { dg-final { scan-assembler {vldr\.16\ts[0-9]+, \[r[0-9]+(, #-?[0-9]+)?\]\n} } } */
/* { dg-final { scan-assembler-not {vldr\.16\t[^\n]*\]!} } } */
/* { dg-final { scan-assembler-not {vstr\.16\t[^\n]*\]!} } } */
