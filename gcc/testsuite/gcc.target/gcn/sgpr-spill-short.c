/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */

/* Scalar values spilled out of VGPRs can be placed in either SGPRs or
   AVGPRs, so both may appear here; the array is large enough that both
   register files fill up and get used.  */
/* { dg-final { scan-assembler "accvgpr" } } */

/* Function epilogues use v6, but other registers may be a spill.  */
/* { dg-final { scan-assembler "v_readlane.+, v\[12345789]" } } */

#define TYPE short

#include "avgpr-spill-int.c"
