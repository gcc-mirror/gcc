/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.5-a" } */

#include <arm_acle.h>

#pragma GCC target("arch=armv8.5-a+memtag")
void foo (int * p)
{
  __arm_mte_set_tag (p);
}
