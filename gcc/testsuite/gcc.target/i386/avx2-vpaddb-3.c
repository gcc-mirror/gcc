/* { dg-do run } */
/* { dg-options "-mavx2 -mno-prefer-avx128 -O2 -ftree-vectorize -save-temps" } */
/* { dg-require-effective-target avx2 } */


#define TYPE char
#define BIN_OP(a, b) ((a) + (b))

#include "avx2-vpop-check.h"

/* { dg-final { scan-assembler-times "vpaddb\[ \\t\]+\[^\n\]*%ymm\[0-9\]" 1 } } */
