/* PR c/69104.  Test atomic routines for invalid memory model errors.  This
   only needs to be tested on a single size.  */
/* { dg-do compile } */
/* { dg-require-effective-target sync_int_long } */

#include <stdatomic.h>

/* atomic_store_explicit():
   The order argument shall not be memory_order_acquire,
   memory_order_consume, nor memory_order_acq_rel.  */

void
store (atomic_int *i)
{
  atomic_store_explicit (i, 0, memory_order_consume); /* { dg-warning "invalid memory model" } */
  atomic_store_explicit (i, 0, memory_order_acquire); /* { dg-warning "invalid memory model" } */
  atomic_store_explicit (i, 0, memory_order_acq_rel); /* { dg-warning "invalid memory model" } */
}

/* atomic_load_explicit():
   The order argument shall not be memory_order_release nor
   memory_order_acq_rel.  */

void
load (atomic_int *i)
{
  atomic_int j = atomic_load_explicit (i, memory_order_release); /* { dg-warning "invalid memory model" } */
  atomic_int k = atomic_load_explicit (i, memory_order_acq_rel); /* { dg-warning "invalid memory model" } */
}

/* atomic_compare_exchange():
   The failure argument shall not be memory_order_release nor
   memory_order_acq_rel.  The failure argument shall be no stronger than the
   success argument.  */

void
exchange (atomic_int *i)
{
  int r;

  atomic_compare_exchange_strong_explicit (i, &r, 0, memory_order_seq_cst, memory_order_release); /* { dg-warning "invalid failure memory" } */
  atomic_compare_exchange_strong_explicit (i, &r, 0, memory_order_seq_cst, memory_order_acq_rel); /* { dg-warning "invalid failure memory" } */
  atomic_compare_exchange_strong_explicit (i, &r, 0, memory_order_relaxed, memory_order_consume); /* { dg-warning "failure memory model cannot be stronger" } */

  atomic_compare_exchange_weak_explicit (i, &r, 0, memory_order_seq_cst, memory_order_release); /* { dg-warning "invalid failure memory" } */
  atomic_compare_exchange_weak_explicit (i, &r, 0, memory_order_seq_cst, memory_order_acq_rel); /* { dg-warning "invalid failure memory" } */
  atomic_compare_exchange_weak_explicit (i, &r, 0, memory_order_relaxed, memory_order_consume); /* { dg-warning "failure memory model cannot be stronger" } */
}

/* atomic_flag_clear():
   The order argument shall not be memory_order_acquire nor
   memory_order_acq_rel.  */

void
clear (atomic_int *i)
{
  atomic_flag_clear_explicit (i, memory_order_acquire); /* { dg-warning "invalid memory model" } */
  atomic_flag_clear_explicit (i, memory_order_acq_rel); /* { dg-warning "invalid memory model" } */
}
