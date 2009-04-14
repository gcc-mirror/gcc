/* { dg-options "-O -Wno-psabi" } */

#ifdef __x86_64__
#include <stdlib.h>
#include "struct-complex-2.h"

void
bar(struct st x)
{
  if (x.s1 != 1
      || __real__ x.x != 2 || __imag__ x.x != 4)
    abort ();
}

void
foo(struct stc x)
{
  if (x.s1 != 1 || x.x.r != 2 || x.x.i != 4)
    abort ();
}
#else
int dummy_y;
#endif
