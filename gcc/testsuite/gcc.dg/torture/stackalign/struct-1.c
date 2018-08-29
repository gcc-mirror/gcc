/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } } */
/* { dg-skip-if "Stack alignment causes use of alloca" { nvptx-*-* } } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

void
foo (void)
{
  struct i
    {
      aligned i;
    } i;

  if (check_int (&i.i,  __alignof__(i.i)) != i.i)
    abort ();
}

int
main()
{
  foo ();
  return 0;
}
