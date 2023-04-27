#define TYPE long
#define N 16
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv16di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv16di3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv16di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16di3@rel32@lo} 0 } } */
