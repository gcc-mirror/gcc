/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "vrolr-template.h"

/* { dg-final { scan-assembler-times {\tvrol\.vv} 4 } } */
/* { dg-final { scan-assembler-times {\tvror\.vv} 4 } } */
