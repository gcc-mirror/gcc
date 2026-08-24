/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler-not "accvgpr" } } */

/* Function epilogues use v6, but other registers may be a spill.  */
/* { dg-final { scan-assembler "v_readlane.+, v\[12345789]" } } */

#ifndef TYPE
#define TYPE int
#endif

#include "avgpr-spill-int.c"
