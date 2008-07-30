/* { dg-do run } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int global;

void
foo (void)
{
  aligned j;

  void bar ()
    {
      aligned i;

      if (check_int (&i,  __alignof__(i)) != i)
	abort ();

      if (check_int (&j,  __alignof__(j)) != j)
	abort ();

      j = -20;
    }
  bar ();

  if (j != -20)
    abort ();

  if (check_int (&j,  __alignof__(j)) != j)
    abort ();
}

int
main()
{
  foo ();
  return 0;
}
