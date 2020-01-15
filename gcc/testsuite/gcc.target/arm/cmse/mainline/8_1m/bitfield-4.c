/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-4.x"

/* { dg-final { scan-assembler "movw\tip, #65535" } } */
/* { dg-final { scan-assembler "movt\tip, 255" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "mov\tip, #255" } } */
/* { dg-final { scan-assembler "and\tr1, r1, ip" } } */
/* { dg-final { scan-assembler "mov\tip, #3" } } */
/* { dg-final { scan-assembler "and\tr2, r2, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "push\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "clrm\t\{r3, r5, r6, r7, r8, r9, r10, fp, ip, APSR\}" } } */
/* { dg-final { scan-assembler "pop\t\{r4, r5, r6, r7, r8, r9, r10, fp\}" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
