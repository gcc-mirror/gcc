/* { dg-do run } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-require-effective-target lzcnt } */

#include "lzcnt-check.h"

int
__attribute__ ((noinline, noclone))
foo (unsigned short a)
{
  return __builtin_clz (a);
}

static void
lzcnt_test ()
{
  int res = foo (1);

  if (res != 31)
    abort ();
}
