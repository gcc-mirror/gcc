#define N 32
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv32si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv32si3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv32si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32si3@rel32@lo} 0 } } */
