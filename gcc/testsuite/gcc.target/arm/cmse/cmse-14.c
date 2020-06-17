/* { dg-do compile } */
/* { dg-options "-mcmse" } */


int __attribute__ ((cmse_nonsecure_call)) (*bar) (void);

int foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r0, )?(r1, )?(r2, )?(r3, )?(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" { target arm_cmse_clear_ok } } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[0-9\]|r10|fp|ip), ){12}APSR\}" { target arm_cmse_clear_ok } } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[0-9\]|r10|fp|ip),\[^\}\]\+\}.*blxns\t\\1" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" { target { ! arm_cmse_clear_ok } } } } */
/* { dg-final { scan-assembler-not "^(.*\\s)?bl?\[^\\s]*\\s+bar" { target { ! arm_cmse_clear_ok } } } } */
/* { dg-final { scan-assembler "blxns" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler-not "^(.*\\s)?bl?(?!xns)\[^\\s]*\\s+bar" { target arm_cmse_clear_ok } } } */
