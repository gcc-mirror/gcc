#define STYPE v16hi
#define UTYPE v16uhi
#define N 16
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv16hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv16hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv16hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv16hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16hi3@rel32@lo} 1 } } */
