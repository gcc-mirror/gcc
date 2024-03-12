#define STYPE v4qi
#define UTYPE v4uqi
#define N 4
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv4qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv4qi4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv4qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv4qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv4qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv4qi3@rel32@lo} 1 } } */
