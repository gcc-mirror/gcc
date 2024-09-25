/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a" } */

#include <arm_acle.h>

data512_t foo (void * p)
{
  return __arm_ld64b (p); /* { dg-error {ACLE function '__arm_ld64b' requires ISA extension 'ls64'} } */
}
