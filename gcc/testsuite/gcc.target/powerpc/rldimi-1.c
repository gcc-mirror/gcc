/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 4045 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 1800 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 447 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rldimi} 892 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 906 } } */


#define SL

#include "rldimi.h"
