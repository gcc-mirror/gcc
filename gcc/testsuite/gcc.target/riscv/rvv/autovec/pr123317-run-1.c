/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-additional-options "-std=c99 -Os --param=gpr2vr-cost=0 -Wno-pedantic" } */

#define OP -=
#define EXPECT 0x881

#include "pr123317-run.h"
