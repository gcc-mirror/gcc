/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

#include "pr100865-4a.c"

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+%(?:r|e)\[^\n\]*, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]%ymm\[0-9\]+, " 2 } } */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastb\[\\t \]+%xmm\[0-9\]+, %ymm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
