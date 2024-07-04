/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#define TYPE long
#include "simd-math-5.c"

/* { dg-final { scan-assembler-times {__divmodv64di4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv64di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv64di3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv64di3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv64di3@rel32@lo} 0 } } */
