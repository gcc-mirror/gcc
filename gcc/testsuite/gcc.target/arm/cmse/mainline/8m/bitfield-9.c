/* { dg-do compile } */
/* { dg-options "-mcmse" } */

#include "../../bitfield-9.x"

/* { dg-final { scan-assembler "movw\tip, #1799" } } */
/* { dg-final { scan-assembler "and\tr0, r0, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "mov\tr1, r4" } } */
/* { dg-final { scan-assembler "mov\tr2, r4" } } */
/* { dg-final { scan-assembler "mov\tr3, r4" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
