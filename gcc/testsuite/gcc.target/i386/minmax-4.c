/* { dg-do compile } */
/* { dg-options "-O2 -mstv -mno-stackrealign -msse4.1" } */

#include "minmax-3.c"

/* { dg-final { scan-assembler-times "pmaxsd" 1 } } */
/* { dg-final { scan-assembler-times "pmaxud" 1 } } */
/* { dg-final { scan-assembler-times "pminsd" 1 } } */
/* { dg-final { scan-assembler-times "pminud" 1 } } */
