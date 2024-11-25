/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "vwsll-template.h"

/* { dg-final { scan-assembler-times {\tvwsll\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvwsll\.vx} 3 } } */
/* { dg-final { scan-assembler-times {\tvwsll\.vi} 3 } } */
