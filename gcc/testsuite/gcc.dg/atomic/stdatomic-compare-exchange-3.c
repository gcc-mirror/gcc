/* Test atomic_compare_exchange routines for existence and proper
   execution on 2-byte values with each valid memory model.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

_Atomic int v = ATOMIC_VAR_INIT (0);
int expected = 0;
int max = ~0;
int desired = ~0;
int zero = 0;

int
main ()
{

  if (!atomic_compare_exchange_strong_explicit (&v, &expected, max, memory_order_relaxed, memory_order_relaxed))
    abort ();
  if (expected != 0)
    abort ();

  if (atomic_compare_exchange_strong_explicit (&v, &expected, 0, memory_order_acquire, memory_order_relaxed))
    abort ();
  if (expected != max)
    abort ();

  if (!atomic_compare_exchange_strong_explicit (&v, &expected, 0, memory_order_release, memory_order_acquire))
    abort ();
  if (expected != max)
    abort ();
  if (v != 0)
    abort ();

  if (atomic_compare_exchange_weak_explicit (&v, &expected, desired, memory_order_acq_rel, memory_order_acquire))
    abort ();
  if (expected != 0)
    abort ();

  if (!atomic_compare_exchange_strong_explicit (&v, &expected, desired, memory_order_seq_cst, memory_order_seq_cst))
    abort ();
  if (expected != 0)
    abort ();
  if (v != max)
    abort ();

  v = 0;

  if (!atomic_compare_exchange_strong (&v, &expected, max))
    abort ();
  if (expected != 0)
    abort ();

  if (atomic_compare_exchange_strong (&v, &expected, zero))
    abort ();
  if (expected != max)
    abort ();

  if (!atomic_compare_exchange_strong (&v, &expected, zero))
    abort ();
  if (expected != max)
    abort ();
  if (v != 0)
    abort ();

  if (atomic_compare_exchange_weak (&v, &expected, desired))
    abort ();
  if (expected != 0)
    abort ();

  if (!atomic_compare_exchange_strong (&v, &expected, desired))
    abort ();
  if (expected != 0)
    abort ();
  if (v != max)
    abort ();

  return 0;
}
