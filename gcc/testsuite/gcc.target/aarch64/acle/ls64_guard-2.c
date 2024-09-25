/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.6-a" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8-a+ls64")
data512_t foo (void * p)
{
  return __arm_ld64b (p);
}
