/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.5-a+memtag -mgeneral-regs-only" } */

#include <arm_acle.h>

void foo (int * p)
{
  __arm_mte_set_tag (p);
}
