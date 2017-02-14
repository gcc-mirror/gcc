/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 1799 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 900 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 870 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rotldi} 27 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 2 } } */


#define CC
#define SL
#define SR

#include "rldicx.h"
