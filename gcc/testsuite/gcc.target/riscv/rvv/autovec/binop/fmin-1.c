/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable --param vect-epilogues-nomask=0 -fno-signaling-nans" } */

#include <stdint-gcc.h>

#define FN(X) __builtin_fmin##X
#include "fmax-1.c"

/* { dg-final { scan-assembler-times {vfmin\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+} 2 } } */

