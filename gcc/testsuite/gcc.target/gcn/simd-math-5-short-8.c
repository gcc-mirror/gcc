#define TYPE short
#define N 8
#include "simd-math-5.c"

/* C integer promotion means that div uses SImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmod8.i4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv8hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv8hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv8hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8hi3@rel32@lo} 0 } } */
