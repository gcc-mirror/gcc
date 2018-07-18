/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 1799 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 900 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 453 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srdi} 27 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 2 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 417 } } */


#define CL
#define SR

#include "rldicx.h"
