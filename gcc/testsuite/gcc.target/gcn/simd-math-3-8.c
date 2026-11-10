#define STYPE v8si
#define UTYPE v8usi
#define N 8
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv8si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv8si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv8si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 1 } } */
