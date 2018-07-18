/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 13977 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 20217 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 6750 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 499 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+mr} 11 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldicl} 6728 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwimi} 1404 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldimi} 134 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwimi} 1270 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 5324 } } */


#define SL

#include "rlwimi.h"
