/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vrem-template.h"

/* Currently we use an epilogue loop which also contains vrems.  Therefore we
   expect 14 vrem[u]s instead of 8.  */

/* { dg-final { scan-assembler-times {\tvrem\.vv} 14 } } */
/* { dg-final { scan-assembler-times {\tvremu\.vv} 14 } } */
