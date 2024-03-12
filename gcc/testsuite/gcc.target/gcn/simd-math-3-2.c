#define STYPE v2si
#define UTYPE v2usi
#define N 2
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv2si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv2si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv2si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv2si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv2si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv2si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 1 } } */
