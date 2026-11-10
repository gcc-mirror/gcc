#define STYPE v8qi
#define UTYPE v8uqi
#define N 8
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv8qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv8qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv8qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8qi3@rel32@lo} 1 } } */
