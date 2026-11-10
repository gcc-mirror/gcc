#define STYPE v4hi
#define UTYPE v4uhi
#define N 4
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv4hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv4hi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv4hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv4hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv4hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv4hi3@rel32@lo} 1 } } */
