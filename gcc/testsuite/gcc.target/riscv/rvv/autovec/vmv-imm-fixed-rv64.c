/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv64gcv -mabi=lp64d -fno-vect-cost-model -mrvv-vector-bits=zvl -fno-builtin" } */

#include "vmv-imm-template.h"

/* { dg-final { scan-assembler-times {vmv.v.i} 32 } } */
/* { dg-final { scan-assembler-times {vmv.v.x} 8 } } */
