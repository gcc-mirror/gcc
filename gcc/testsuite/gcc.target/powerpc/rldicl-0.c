/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 1799 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 900 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 841 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rotldi} 29 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srdi} 27 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 2 } } */


#define CL
#define SL
#define SR

#include "rldicx.h"
