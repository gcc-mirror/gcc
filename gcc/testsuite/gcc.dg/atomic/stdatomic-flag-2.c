/* Test atomic_flag routines for existence and execution.  Out-of-line
   function calls.  */
/* The test needs a lockless atomic implementation.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);
atomic_flag a = ATOMIC_FLAG_INIT;

int
main ()
{
  int b;

  if (!atomic_is_lock_free (&a))
    abort ();

  if ((atomic_flag_test_and_set) (&a))
    abort ();
  (atomic_flag_clear_explicit) (&a, memory_order_relaxed);
  if ((atomic_flag_test_and_set) (&a))
    abort ();
  (atomic_flag_clear) (&a);

  b = (atomic_flag_test_and_set_explicit) (&a, memory_order_seq_cst);
  if (!(atomic_flag_test_and_set) (&a) || b != 0)
    abort ();

  b = (atomic_flag_test_and_set_explicit) (&a, memory_order_acq_rel);
  if (!(atomic_flag_test_and_set) (&a) || b != 1)
    abort ();

  (atomic_flag_clear_explicit) (&a, memory_order_seq_cst);
  if ((atomic_flag_test_and_set) (&a))
    abort ();

  return 0;
}
