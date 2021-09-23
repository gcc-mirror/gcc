/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake -mno-avx2" } */

#include "pr100865-12a.c"

/* { dg-final { scan-assembler-times "movabsq" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklqdq\[\\t \]+\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[\\t \]%xmm\[0-9\]+, " 16 } } */
