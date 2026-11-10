#define TYPE char
#define N 16
#include "simd-math-5.c"

/* C integer promotion means that div uses HImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmod16.i4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv16qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv16qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv16qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16qi3@rel32@lo} 0 } } */
