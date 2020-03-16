/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mno-avx512vl" } */

#include "pr89229-7a.c"

/* { dg-final { scan-assembler-times "vmovdqa32\[^\n\r]*zmm1\[67]\[^\n\r]*zmm1\[67]" 1 } } */
