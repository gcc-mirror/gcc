/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake-avx512" } */

#include "pr100865-8a.c"

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[\\t \]%xmm\[0-9\]+, " 16 } } */
