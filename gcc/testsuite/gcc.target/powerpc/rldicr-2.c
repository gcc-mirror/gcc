/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 1799 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 900 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 29 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 870 } } */


#define CR
#define SR

#include "rldicx.h"
