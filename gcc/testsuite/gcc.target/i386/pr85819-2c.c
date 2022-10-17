/* { dg-do compile } */
/* { dg-options "-O2 -mno-fma -mno-avx2 -mno-avx512vl -mavx512f -mfpmath=sse" } */

#include "pr85819-2a.c"

/* { dg-final { scan-assembler-not "vcvtudq2ps" } } */
/* { dg-final { scan-assembler-not "vfmadd132ps" } } */
