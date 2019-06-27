/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-mmx -mavx512bw -mno-avx512vl" } */
/* { dg-final { scan-assembler-times "pshufd" 1 } } */
/* { dg-final { scan-assembler-times "movd" 1 } } */
/* { dg-final { scan-assembler-not "movl" } } */

#include "sse2-mmx-18a.c"
