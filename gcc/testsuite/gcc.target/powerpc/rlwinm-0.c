/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 6739 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 9716 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 3375 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldicl} 3090 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 3197 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 3084 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rotlwi} 154 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srwi} 13 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srdi} 13 { target lp64 } } } */


#define SL
#define SR

#include "rlwinm.h"
