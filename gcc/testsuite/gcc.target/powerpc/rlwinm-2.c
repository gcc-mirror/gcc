/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 6739 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 9466 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 3375 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 2840 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 833 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 721 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srwi} 13 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+srdi} 12 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 2518 } } */


#define SR

#include "rlwinm.h"
