/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "clz-template.h"

/* { dg-final { scan-assembler-times {\tvclz.v} 8 } } */
