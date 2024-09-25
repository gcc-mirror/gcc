/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.5-a+memtag" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8.5-a")
void foo (int * p)
{
  __arm_mte_set_tag (p); /* { dg-error {ACLE function '__arm_mte_set_tag' requires ISA extension 'memtag'} } */
}
