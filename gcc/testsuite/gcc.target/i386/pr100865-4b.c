/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

#include "pr100865-4a.c"

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+%(?:r|e)\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]%xmm\[0-9\]+, " 4 } } */
/* { dg-final { scan-assembler-not "vpbroadcastb\[\\t \]+%xmm\[0-9\]+, %xmm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
