#define STYPE v2hi
#define UTYPE v2uhi
#define N 2
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv2hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv2hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv2hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv2hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv2hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv2hi3@rel32@lo} 1 } } */
