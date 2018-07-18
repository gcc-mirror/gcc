/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 4471 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 1800 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 873 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldimi} 1744 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rotldi} 54 } } */


#define SL
#define SR

#include "rldimi.h"
