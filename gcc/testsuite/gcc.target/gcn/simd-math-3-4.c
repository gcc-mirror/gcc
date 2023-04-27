#define STYPE v4si
#define UTYPE v4usi
#define N 4
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv4si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv4si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv4si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv4si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv4si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv4si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 1 } } */
