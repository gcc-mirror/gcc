/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512" } */

#include "pr100865-5a.c"

/* { dg-final { scan-assembler-times "vpbroadcastw\[\\t \]+%(?:r|e)\[^\n\]*, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[\\t \]%ymm\[0-9\]+, " 4 } } */
/* { dg-final { scan-assembler-not "vpbroadcastw\[\\t \]+%xmm\[0-9\]+, %ymm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
