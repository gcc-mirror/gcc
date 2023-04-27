#define STYPE v8hi
#define UTYPE v8uhi
#define N 8
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv8hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv8hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv8hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8hi3@rel32@lo} 1 } } */
