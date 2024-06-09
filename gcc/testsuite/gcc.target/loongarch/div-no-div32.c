/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "div\.w" } } */
/* { dg-final { scan-assembler "div\.wu" } } */
/* { dg-final { scan-assembler "mod\.w" } } */
/* { dg-final { scan-assembler "mod\.wu" } } */

/* -mno-div32 should be implied by -march=loongarch64.  */
/* { dg-final { scan-assembler-times "slli\.w\[^\n\]*0" 8 } } */

#include "div-div32.c"
