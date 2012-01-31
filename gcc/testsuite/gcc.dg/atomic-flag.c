/* Test __atomic routines for existence and execution.  */
/* { dg-do run } */

/* Test that __atomic_test_and_set and __atomic_clear builtins execute.  */

extern void abort(void);
unsigned char a;

main ()
{
  int b;

  __atomic_clear (&a, __ATOMIC_RELAXED);
  if (a != 0)
    abort ();

  b = __atomic_test_and_set (&a, __ATOMIC_SEQ_CST);
  if (a != __GCC_ATOMIC_TEST_AND_SET_TRUEVAL || b != 0)
    abort ();

  b = __atomic_test_and_set (&a, __ATOMIC_ACQ_REL);
  if (a != __GCC_ATOMIC_TEST_AND_SET_TRUEVAL || b != 1)
    abort ();

  __atomic_clear (&a, __ATOMIC_SEQ_CST);
  if (a != 0)
    abort ();

  return 0;
}
