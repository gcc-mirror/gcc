/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vdiv-template.h"

/* Currently we use an epilogue loop which also contains vdivs.  Therefore we
   expect 10 vdiv[u]s instead of 6.  */

/* { dg-final { scan-assembler-times {\tvdiv\.vv} 10 } } */
/* { dg-final { scan-assembler-times {\tvdivu\.vv} 10 } } */
