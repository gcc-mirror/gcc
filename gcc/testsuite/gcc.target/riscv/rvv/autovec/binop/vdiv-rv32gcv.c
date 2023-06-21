/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax -ffast-math" } */

#include "vdiv-template.h"

/* Currently we use an epilogue loop which also contains vdivs.  Therefore we
   expect 14 vdiv[u]s instead of 8.  */

/* { dg-final { scan-assembler-times {\tvdiv\.vv} 14 } } */
/* { dg-final { scan-assembler-times {\tvdivu\.vv} 14 } } */

/* Division by constant is done by calculating a reciprocal and
   then multiplying.  Hence we do not expect 6 vfdivs.  */
/* { dg-final { scan-assembler-times {\tvfdiv\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvfmul\.vv} 3 } } */
