/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -fno-vect-cost-model" } */

#include "reduc_call-1.c"

/* { dg-final { scan-assembler {vfmacc\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} } } */
