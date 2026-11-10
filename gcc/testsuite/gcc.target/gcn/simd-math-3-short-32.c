#define STYPE v32hi
#define UTYPE v32uhi
#define N 32
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv32hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv32hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv32hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv32hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32hi3@rel32@lo} 1 } } */
