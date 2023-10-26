/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a" } */

#include <arm_acle.h>

void foo (void)
{
  __tcommit (); /* { dg-error {ACLE function '__tcommit' requires ISA extension 'tme'} } */
}
