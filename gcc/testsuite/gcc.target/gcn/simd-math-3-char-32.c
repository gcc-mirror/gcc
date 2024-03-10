#define STYPE v32qi
#define UTYPE v32uqi
#define N 32
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv32qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv32qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv32qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv32qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32qi3@rel32@lo} 1 } } */
