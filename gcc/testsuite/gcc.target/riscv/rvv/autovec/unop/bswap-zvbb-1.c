/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -fdump-tree-vect-details" } */

#include "bswap-zvbb-template.h"

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 3 "vect" } } */
/* { dg-final { scan-assembler-times {\tvrev8\.v} 3 } } */
/* { dg-final { scan-assembler-not {\tvbrev8\.v} } } */
/* { dg-final { scan-assembler-not {\tvrgather} } } */
/* { dg-final { scan-assembler-not {\tvsll\.v} } } */
/* { dg-final { scan-assembler-not {\tvsrl\.v} } } */
