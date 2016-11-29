/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 16485 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 19909 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 6750 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 3007 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 11 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldicl} 6420 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwimi} 6420 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldimi} 310 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwimi} 6110 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rotlwi} 308 } } */


#define SL
#define SR

#include "rlwimi.h"
