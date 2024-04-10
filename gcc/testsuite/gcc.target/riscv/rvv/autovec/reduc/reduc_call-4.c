/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -fno-vect-cost-model -ffast-math" } */

#include "reduc_call-1.c"

/* { dg-final { scan-assembler {vfmadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+} } } */
/* { dg-final { scan-assembler {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
