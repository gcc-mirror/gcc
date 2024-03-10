#define STYPE v2qi
#define UTYPE v2uqi
#define N 2
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv2qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv2qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv2qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv2qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv2qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv2qi3@rel32@lo} 1 } } */
