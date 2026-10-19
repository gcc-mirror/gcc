/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_partial_vectors } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

struct data
{
  unsigned int list[16];
  unsigned int sentinel;
};

static struct data data __attribute__ ((aligned (64))) =
{
  { 0 },
  1
};

__attribute__ ((noinline, noipa))
static int
contains (const unsigned int *p, unsigned int count, unsigned int start,
          unsigned int key)
{
  for (unsigned int i = start; i < count; ++i)
    if (p[i] == key)
      return 1;

  return 0;
}

int
main (void)
{
  check_vect ();

  if (contains (data.list, 2, 1, 1) != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
