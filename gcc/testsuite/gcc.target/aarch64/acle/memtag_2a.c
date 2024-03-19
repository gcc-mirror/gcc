/* Test the MEMTAG intrinsic qualifier warnings and argument errors.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -march=armv8.5-a+memtag" } */

#include "arm_acle.h"

void
test_memtag_warning_return_qualifier (void)
{
  const char *c;
  volatile char *v;
  char *n;
  int *i;
  int64_t d;

  v = __arm_mte_get_tag(c);		  /* { dg-warning {assignment} } */
  n = __arm_mte_get_tag(c);		  /* { dg-warning {assignment} } */
  i = __arm_mte_get_tag(c);		  /* { dg-error {assignment} } */
  c = __arm_mte_get_tag(v);		  /* { dg-warning {assignment} } */
  n = __arm_mte_get_tag(v);		  /* { dg-warning {assignment} } */

  i = __arm_mte_create_random_tag (c, 0); /* { dg-error {assignment} } */
  i = __arm_mte_increment_tag (c, 0);	  /* { dg-error {assignment} } */

  c = __arm_mte_get_tag(n);		  /* No warning.  */
  d = __arm_mte_ptrdiff(c, i);		  /* No warning.  */
}

void
test_memtag_warning_argument (void)
{
  const char *c;
  __arm_mte_exclude_tag(0, 0);		/* No warning.  */
  __arm_mte_create_random_tag (0, 0);	/* No warning.  */
  __arm_mte_set_tag(0);			/* No warning.  */
  __arm_mte_get_tag(0);			/* No warning.  */
  __arm_mte_increment_tag (0, 15);	/* No warning.  */
  __arm_mte_ptrdiff(c, 0);		/* No warning.  */
  __arm_mte_ptrdiff(0, c);		/* No warning.  */
}

void
test_memtag_error_argument (void)
{
  /* Produce errors properly for invalid arguments.  */
  __arm_mte_exclude_tag(no_decl, 0);	/* { dg-error {} } */
  __arm_mte_exclude_tag();		/* { dg-error {} } */
  __arm_mte_ptrdiff(no_decl2, 0);	/* { dg-error {} } */
  __arm_mte_ptrdiff(0);			/* { dg-error {} } */
  __arm_mte_ptrdiff();			/* { dg-error {} } */

  const char *c;
  uint64_t i;
  __arm_mte_exclude_tag(i, 0);		/* { dg-error {argument} } */
  __arm_mte_create_random_tag (i, 0);	/* { dg-error {argument} } */
  __arm_mte_set_tag(i);			/* { dg-error {argument} } */
  __arm_mte_get_tag(i);			/* { dg-error {argument} } */
  __arm_mte_increment_tag (i, 15);	/* { dg-error {argument} } */
  __arm_mte_ptrdiff(c, i);		/* { dg-error {argument} } */
  __arm_mte_ptrdiff(i, c);		/* { dg-error {argument} } */

  __arm_mte_exclude_tag(1, 0);		/* { dg-error {argument} } */
  __arm_mte_create_random_tag (1, 0);	/* { dg-error {argument} } */
  __arm_mte_set_tag(1);			/* { dg-error {argument} } */
  __arm_mte_get_tag(1);			/* { dg-error {argument} } */
  __arm_mte_increment_tag (1, 15);	/* { dg-error {argument} } */
  __arm_mte_ptrdiff(c, 1);		/* { dg-error {argument} } */
  __arm_mte_ptrdiff(1, c);		/* { dg-error {argument} } */
}
