#define TYPE char
#define N 4
#include "simd-math-5.c"

/* C integer promotion means that div uses HImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmod4.i4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv4hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv4qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv4qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv4qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv4qi3@rel32@lo} 0 } } */
