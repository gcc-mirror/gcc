#define STYPE v2di
#define UTYPE v2udi
#define N 2
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv2di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv2di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv2di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv2di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv2di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv2di3@rel32@lo} 1 } } */
