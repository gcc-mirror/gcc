/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -O2" } */

#include "bswap-zvbb-perm-template.h"

/* { dg-final { scan-assembler-times {\tvrev8\.v} 3 } } */
/* { dg-final { scan-assembler-not {\tvbrev8\.v} } } */
