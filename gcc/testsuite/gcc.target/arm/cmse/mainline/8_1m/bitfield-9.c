/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-9.x"

/* { dg-final { scan-assembler "movw\tip, #1799" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "clrm\t\{r1, r2, r3, r5, r6, r7, r8, r9, r10, fp, ip, APSR\}" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
