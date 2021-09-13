/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake -mno-avx2" } */

#include "pr100865-9a.c"

/* { dg-final { scan-assembler-times "vpshufd\[\\t \]+\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[\\t \]%xmm\[0-9\]+, " 16 } } */
