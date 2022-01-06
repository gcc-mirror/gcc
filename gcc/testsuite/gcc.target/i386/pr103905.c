/* PR target/103905 */
/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O3 -mxop" } */

#include "xop-check.h"

char perm[64];

void
__attribute__((noipa))
foo (int n)
{
  for (int i = 0; i < n; ++i)
    perm[i] = i;
}

static void
xop_test (void)
{
  foo (8);

  if (perm[7] != 7)
    __builtin_abort ();
}
