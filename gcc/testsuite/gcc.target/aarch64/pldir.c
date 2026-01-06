/* { dg-do compile } */
/* { dg-options "-march=armv8-a -O2" } */

#include <arm_acle.h>

void
prefetch_intent_to_read (void *addr)
{
  __pldir (addr);
}

/* { dg-final { scan-assembler "prfm\tir, \[x\[0-9\]+\]" } } */