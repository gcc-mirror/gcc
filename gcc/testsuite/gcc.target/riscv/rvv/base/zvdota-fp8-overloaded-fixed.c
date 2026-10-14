/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfqwdota8f_zvl128b -mabi=lp64d -mrvv-vector-bits=zvl -O2" } */

#define FIXED_RVV
#include "zvdota-fp8-overloaded.c"

/* { dg-final { scan-assembler-times {vfqwdota\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfqwdota\.alt\.vv} 2 } } */
