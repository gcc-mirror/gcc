#define TYPE long
#define N 32
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv32di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv32di3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv32di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32di3@rel32@lo} 0 } } */
