#define STYPE v16qi
#define UTYPE v16uqi
#define N 16
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv16qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv16qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv16qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv16qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16qi3@rel32@lo} 1 } } */
