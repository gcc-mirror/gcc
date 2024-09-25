/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+ls64" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8.6-a")
data512_t foo (void * p)
{
  return __arm_ld64b (p); /* { dg-error {ACLE function '__arm_ld64b' requires ISA extension 'ls64'} } */
}
