/* { dg-do compile } */
/* { dg-options "-O2 -mstv -mno-stackrealign -mavx512vl" } */

#include "minmax-3.c"

/* { dg-final { scan-assembler-times "vpmaxsd" 1 } } */
/* { dg-final { scan-assembler-times "vpmaxud" 1 } } */
/* { dg-final { scan-assembler-times "vpminsd" 1 } } */
/* { dg-final { scan-assembler-times "vpminud" 1 } } */
/* { dg-final { scan-assembler-times "vpmaxsq" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "vpmaxuq" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "vpminsq" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "vpminuq" 1 { target lp64 } } } */
