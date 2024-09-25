/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+tme -mgeneral-regs-only" } */

#include <arm_acle.h>

void foo (void)
{
  __tcommit ();
}
