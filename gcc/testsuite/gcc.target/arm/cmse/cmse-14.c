/* { dg-do compile } */
/* { dg-options "-mcmse" } */


int __attribute__ ((cmse_nonsecure_call)) (*bar) (void);

int foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "clrm\t\{r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, fp, ip, APSR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
/* { dg-final { scan-assembler-not "^(.*\\s)?bl?\[^\\s]*\\s+bar" } } */
