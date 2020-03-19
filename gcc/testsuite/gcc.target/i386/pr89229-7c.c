/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mprefer-vector-width=512" } */

#include "pr89229-7a.c"

/* { dg-final { scan-assembler-times "vmovdqa32\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" 1 } } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
