#define STYPE v16si
#define UTYPE v16usi
#define N 16
#include "simd-math-3.c"

/* { dg-final { scan-assembler-times {__divmodv16si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv16si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv16si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 1 } } */
