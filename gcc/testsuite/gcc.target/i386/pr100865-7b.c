/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512" } */

#include "pr100865-7a.c"

/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %ymm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+\[^\n\]*, %ymm\[0-9\]+" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 16 } } */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
