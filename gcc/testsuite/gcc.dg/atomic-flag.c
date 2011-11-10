/* Test __atomic routines for existence and execution.  */
/* { dg-do run } */

#include <stdbool.h>

/* Test that __atomic_test_and_set and __atomic_clear builtins execute.  */

extern void abort(void);
bool a;

main ()
{
  bool b;

  __atomic_clear (&a, __ATOMIC_RELAXED);
  if (a != 0)
    abort ();

  b = __atomic_test_and_set (&a, __ATOMIC_SEQ_CST);
  if (a != 1 || b != 0)
    abort ();

  b = __atomic_test_and_set (&a, __ATOMIC_ACQ_REL);
  if (b != 1 || a != 1)
    abort ();

  __atomic_clear (&a, __ATOMIC_SEQ_CST);
  if (a != 0)
    abort ();

  return 0;
}
