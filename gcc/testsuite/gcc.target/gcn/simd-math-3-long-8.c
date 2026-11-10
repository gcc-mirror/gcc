#define STYPE v8di
#define UTYPE v8udi
#define N 8
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv8di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv8di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv8di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8di3@rel32@lo} 1 } } */
