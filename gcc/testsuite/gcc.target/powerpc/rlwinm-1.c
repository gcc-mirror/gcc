/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 6739 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 9496 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+blr} 3375 } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rldic} 2847 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 691 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+rlwinm} 612 { target lp64 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+slwi} 11 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {(?n)^\s+slwi} 0 { target lp64 } } } */

/* { dg-final { scan-assembler-times {(?n)^\s+mulli} 2662 { target ilp32 } } } */


#define SL

#include "rlwinm.h"
