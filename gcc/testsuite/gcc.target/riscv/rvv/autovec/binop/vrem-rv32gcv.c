/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vrem-template.h"

/* Currently we use an epilogue loop which also contains vrems.  Therefore we
   expect 10 vrem[u]s instead of 6.  */

/* { dg-final { scan-assembler-times {\tvrem\.vv} 10 } } */
/* { dg-final { scan-assembler-times {\tvremu\.vv} 10 } } */
