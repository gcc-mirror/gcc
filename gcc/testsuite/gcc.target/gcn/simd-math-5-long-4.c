#define TYPE long
#define N 4
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv4di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv4di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv4di3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv4di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv4di3@rel32@lo} 0 } } */
