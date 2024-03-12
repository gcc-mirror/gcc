#define N 16
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv16si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv16si3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16si3@rel32@lo} 0 } } */
