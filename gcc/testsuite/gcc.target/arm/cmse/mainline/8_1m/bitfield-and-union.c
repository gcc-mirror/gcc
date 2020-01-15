/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-and-union.x"

/* { dg-final { scan-assembler "movw\tip, #7939" } } */
/* { dg-final { scan-assembler "movt\tip, 15" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "movw\tip, #65535" } } */
/* { dg-final { scan-assembler "movt\tip, 2047" } } */
/* { dg-final { scan-assembler "and\tr1, r1, ip" } } */
/* { dg-final { scan-assembler "mov\tip, #1" } } */
/* { dg-final { scan-assembler "movt\tip, 65535" } } */
/* { dg-final { scan-assembler "and\tr2, r2, ip" } } */
/* { dg-final { scan-assembler "movw\tip, #65535" } } */
/* { dg-final { scan-assembler "movt\tip, 31" } } */
/* { dg-final { scan-assembler "and\tr3, r3, ip" } } */
/* Shift on the same register as blxns.  */
/* { dg-final { scan-assembler "lsrs\t(r\[4-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "lsls\t(r\[4-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[4-9\]|r10|fp|ip), ){8}APSR\}" } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[4-9\]|r10|fp|ip),\[^\}\]\+\}.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "blxns" } } */
