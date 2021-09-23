/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512 -mfpmath=sse" } */

#include "pr88531-2a.c"

/* { dg-final { scan-assembler-times "vmulps" 2 } } */
