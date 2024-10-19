/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model -mno-autovec-segment" } */

#include "struct_vect-13.c"

/* { dg-final { scan-assembler-not {v[ls]seg[2-8]e[123468]+\.v} } } */
