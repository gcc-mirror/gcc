/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8-a+tme")
void foo (void)
{
  __tcommit ();
}
