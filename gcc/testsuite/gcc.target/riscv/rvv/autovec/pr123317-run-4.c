/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-additional-options "-std=c99 -O2 --param=gpr2vr-cost=0 -Wno-pedantic" } */

#define OP +=
#define EXPECT 0x350000000000005c

#include "pr123317-run.h"
