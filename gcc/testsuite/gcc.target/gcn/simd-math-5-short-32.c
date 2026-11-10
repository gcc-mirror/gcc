#define TYPE short
#define N 32
#include "simd-math-5.c"

/* C integer promotion means that div uses SImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmod32.i4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv32hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv32hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv32hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32hi3@rel32@lo} 0 } } */
