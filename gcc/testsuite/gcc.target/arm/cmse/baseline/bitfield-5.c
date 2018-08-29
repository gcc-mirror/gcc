/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../bitfield-5.x"

/* { dg-final { scan-assembler "mov\tip, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #8191" } } */
/* { dg-final { scan-assembler "movt\tr4, 255" } } */
/* { dg-final { scan-assembler "ands\tr0, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #2047" } } */
/* { dg-final { scan-assembler "ands\tr1, r4" } } */
/* { dg-final { scan-assembler "mov\tr4, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "movs\tr2, r4" } } */
/* { dg-final { scan-assembler "movs\tr3, r4" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */

