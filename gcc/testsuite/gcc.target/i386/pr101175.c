/* { dg-do run } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-require-effective-target lzcnt } */

#include "lzcnt-check.h"

static int
foo (unsigned int v)
{
  return v ? __builtin_clz (v) : 32;
}

/* returns -1 if x == 0 */
int
__attribute__ ((noinline, noclone))
bar (unsigned int x)
{
  return 31 - foo (x);
}

static void
lzcnt_test ()
{
  int r = bar (0);

  if (r != -1)
    abort ();
}
