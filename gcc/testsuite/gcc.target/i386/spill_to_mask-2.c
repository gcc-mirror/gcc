/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

#ifndef DTYPE
#define DTYPE u16
#endif

#include "spill_to_mask-1.c"

/* { dg-final { scan-assembler "kmovw" } } */
