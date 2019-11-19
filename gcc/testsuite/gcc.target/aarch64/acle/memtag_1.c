/* Test the MEMTAG ACLE intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -march=armv8.5-a+memtag" } */

#include "arm_acle.h"

/* irg */

void *
test_memtag_1 (void *p)
{
  return __arm_mte_create_random_tag (p, 0);
}

/* gmi */

uint64_t
test_memtag_2 (void *p)
{
  return __arm_mte_exclude_tag (p, 0);
}

/* addg */

void *
test_memtag_3 (void *p)
{
  return __arm_mte_increment_tag (p, 1);
}

/* subp */

int64_t
test_memtag_4 (void *p, void *q)
{
  return __arm_mte_ptrdiff (p, q);
}

/* ldg */

void *
test_memtag_5 (void *p)
{
  return __arm_mte_get_tag (p);
}

/* stg */

void
test_memtag_6 (void *p)
{
  __arm_mte_set_tag (p);
}

/* { dg-final { scan-assembler-times {irg\tx..?, x..?, x..?\n} 1 } } */
/* { dg-final { scan-assembler-times {gmi\tx..?, x..?, x..?\n} 1 } } */
/* { dg-final { scan-assembler-times {subp\tx..?, x..?, x..?\n} 1 } } */
/* { dg-final { scan-assembler-times {addg\tx..?, x..?, #0, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {ldg\tx..?, \[x..?, #0\]\n} 1 } } */
/* { dg-final { scan-assembler-times {stg\tx..?, \[x..?, #0\]\n} 1 } } */