#define STYPE v32di
#define UTYPE v32udi
#define N 32
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv32di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv32di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv32di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv32di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv32di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv32di3@rel32@lo} 1 } } */
