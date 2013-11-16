/* Test atomic_kill_dependency.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

_Atomic int a = ATOMIC_VAR_INIT (1), b;

int
main ()
{
  b = kill_dependency (a);
  if (b != 1)
    abort ();

  return 0;
}
