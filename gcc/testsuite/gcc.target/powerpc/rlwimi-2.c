/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 14121 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 20217 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 6750 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 643 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 11 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldicl} 6728 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwimi} 1692 } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 5036 } } */


#define SR

#include "rlwimi.h"
