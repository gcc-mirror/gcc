/* { dg-do compile } */
/* { dg-options "-mcmse" }  */
/* { dg-require-effective-target arm_arch_v8m_base_ok } */
/* { dg-add-options arm_arch_v8m_base } */

extern float bar (void);

float __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return bar ();
}
/* { dg-final { scan-assembler "movs\tr1, r0" } } */
/* { dg-final { scan-assembler "movs\tr2, r0" } } */
/* { dg-final { scan-assembler "movs\tr3, r0" } } */
/* { dg-final { scan-assembler "mov\tip, r0" } } */
/* { dg-final { scan-assembler "mov\tlr, r0" } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq," } } */
/* { dg-final { scan-assembler "bxns" } } */
