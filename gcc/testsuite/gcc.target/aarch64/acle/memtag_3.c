/* Test the MEMTAG intrinsic expanding errors.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -march=armv8.5-a+memtag" } */

#include "arm_acle.h"

void
test_memtag_error_expand (int i)
{
  const char *p;
  p = __arm_mte_increment_tag (p, -1);	/* { dg-error {in range \[0,15\]} } */
  p = __arm_mte_increment_tag (p, 16);	/* { dg-error {in range \[0,15\]} } */
  p = __arm_mte_increment_tag (p, i);	/* { dg-error {constant immediate} } */
}