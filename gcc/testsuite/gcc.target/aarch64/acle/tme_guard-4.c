/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+tme" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8-a")
void foo (void)
{
  __tcommit (); /* { dg-error {ACLE function '__tcommit' requires ISA extension 'tme'} } */
}
