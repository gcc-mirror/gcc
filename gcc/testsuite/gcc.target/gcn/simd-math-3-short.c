#define STYPE v64hi
#define UTYPE v64uhi
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv64hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv64hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv64hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv64hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv64hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv64hi3@rel32@lo} 1 } } */
