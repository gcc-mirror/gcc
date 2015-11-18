/* Test atomic_*_fence routines for existence and execution with each
   valid memory model.  Out-of-line function calls.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

int
main ()
{
  (atomic_thread_fence) (memory_order_relaxed);
  (atomic_thread_fence) (memory_order_consume);
  (atomic_thread_fence) (memory_order_acquire);
  (atomic_thread_fence) (memory_order_release);
  (atomic_thread_fence) (memory_order_acq_rel);
  (atomic_thread_fence) (memory_order_seq_cst);

  (atomic_signal_fence) (memory_order_relaxed);
  (atomic_signal_fence) (memory_order_consume);
  (atomic_signal_fence) (memory_order_acquire);
  (atomic_signal_fence) (memory_order_release);
  (atomic_signal_fence) (memory_order_acq_rel);
  (atomic_signal_fence) (memory_order_seq_cst);

  return 0;
}
