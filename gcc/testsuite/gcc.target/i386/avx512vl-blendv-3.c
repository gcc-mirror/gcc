/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times {vp?blendv(?:b|p[sd])[ \t]*} 6 } } */
/* { dg-final { scan-assembler-not {vpcmp} } } */

#include "blendv-3.c"
