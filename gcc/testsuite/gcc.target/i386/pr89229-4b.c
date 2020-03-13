/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mno-avx512vl" } */

#include "pr89229-4a.c"

/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "vmovapd" } } */
