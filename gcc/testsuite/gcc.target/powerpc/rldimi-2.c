/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 3628 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 1800 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 30 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldimi} 58 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 1740 } } */


#define SR

#include "rldimi.h"
