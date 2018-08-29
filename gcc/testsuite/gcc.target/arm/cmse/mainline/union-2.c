/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../union-2.x"

/* { dg-final { scan-assembler "movw\tip, #8191" } } */
/* { dg-final { scan-assembler "movt\tip, 63" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "movw\tip, #511" } } */
/* { dg-final { scan-assembler "movt\tip, 65535" } } */
/* { dg-final { scan-assembler "and\tr1, r1, ip" } } */
/* { dg-final { scan-assembler "movw\tip, #65535" } } */
/* { dg-final { scan-assembler "movt\tip, 31" } } */
/* { dg-final { scan-assembler "and\tr2, r2, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "mov\tr3, r4" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
