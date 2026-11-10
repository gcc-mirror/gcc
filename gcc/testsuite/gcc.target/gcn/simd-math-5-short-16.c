#define TYPE short
#define N 16
#include "simd-math-5.c"

/* C integer promotion means that div uses SImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmod16.i4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv16hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv16hi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv16hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16hi3@rel32@lo} 0 } } */
