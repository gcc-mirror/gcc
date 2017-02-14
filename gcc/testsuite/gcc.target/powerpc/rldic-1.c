/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 1799 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 900 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 415 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+sldi} 29 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 2 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 453 } } */


#define CC
#define SL

#include "rldicx.h"
