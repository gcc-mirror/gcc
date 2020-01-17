/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-6.x"

/* { dg-final { scan-assembler "movw\tip, #65535" } } */
/* { dg-final { scan-assembler "movt\tip, 1023" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "mov\tip, #3" } } */
/* { dg-final { scan-assembler "movt\tip, 32767" } } */
/* { dg-final { scan-assembler "and\tr1, r1, ip" } } */
/* { dg-final { scan-assembler "mov\tip, #255" } } */
/* { dg-final { scan-assembler "and\tr2, r2, ip" } } */
/* Shift on the same register as blxns.  */
/* { dg-final { scan-assembler "lsrs\t(r\[3-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "lsls\t(r\[3-9\]|r10|fp|ip), \\1, #1.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* Check the right registers are cleared and none appears twice.  */
/* { dg-final { scan-assembler "clrm\t\{(r3, )?(r4, )?(r5, )?(r6, )?(r7, )?(r8, )?(r9, )?(r10, )?(fp, )?(ip, )?APSR\}" } } */
/* Check that the right number of registers is cleared and thus only one
   register is missing.  */
/* { dg-final { scan-assembler "clrm\t\{((r\[3-9\]|r10|fp|ip), ){9}APSR\}" } } */
/* Check that no cleared register is used for blxns.  */
/* { dg-final { scan-assembler-not "clrm\t\{\[^\}\]\+(r\[3-9\]|r10|fp|ip),\[^\}\]\+\}.*blxns\t\\1" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "blxns" } } */
