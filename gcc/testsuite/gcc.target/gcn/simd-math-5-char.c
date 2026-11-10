/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#define TYPE char
#include "simd-math-5.c"

/* C integer promotion means that div uses HImode and divmod doesn't match.  */
/* { dg-final { scan-assembler-times {__divmodv64si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv64hi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divv64qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__udivv64qi3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv64qi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv64qi3@rel32@lo} 0 } } */
