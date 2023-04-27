#define TYPE long
#define N 8
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv8di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8di3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv8di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8di3@rel32@lo} 0 } } */
