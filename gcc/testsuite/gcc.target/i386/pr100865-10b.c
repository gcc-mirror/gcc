/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake-avx512" } */

#include "pr100865-10a.c"

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]%ymm\[0-9\]+, " 8 } } */
