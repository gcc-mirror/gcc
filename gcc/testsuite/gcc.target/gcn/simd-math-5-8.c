#define N 8
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv8si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8si3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8si3@rel32@lo} 0 } } */
