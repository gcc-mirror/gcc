/* { dg-do run { target ia32 } } */
/* { dg-require-effective-target sse2_runtime } */
/* { dg-options "-O2 -msse2 -mstv -W" } */

#include <stdlib.h>
#include "pr95021-1.c"

jmp_buf buf;

long long *target_p;
long long *c;

int count;

__attribute__ ((noclone, noinline))
void
foo (long long x)
{
  if (x != *c)
    abort ();
  if (!count)
    {
      count++;
      longjmp (buf, 1);
    }
}

int
main ()
{
  long long val = 30;
  long long local = 0;
  target_p = &val;
  c = &local;
  bar ();
  if (val != local)
    abort ();
  return 0;
}
