/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fno-signaling-nans" } */

#include <stdint-gcc.h>

#define FN(X) __builtin_fmin##X
#include "cond_fmax-4.c"

/* { dg-final { scan-assembler-times {vfmin\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 6 } } */
/* { dg-final { scan-assembler-not {vfmin\.vf\s+v[0-9]+,v[0-9]+,fa[0-9],v0.t} } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
