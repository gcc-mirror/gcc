/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-9.x"

/* { dg-final { scan-assembler "movw\tip, #1799" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* Shift on the same register as blxns.  */
/* { dg-final { scan-assembler "lsrs\t(r\[1-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "lsls\t(r\[1-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r1, )?(r2, )?(r3, )?(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[1-9\]|r10|fp|ip), ){11}APSR\}" } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[1-9\]|r10|fp|ip),\[^\}\]\+\}.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "blxns" } } */
