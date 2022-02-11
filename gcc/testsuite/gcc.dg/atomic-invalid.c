/* Test __atomic routines for invalid memory model errors. This only needs
   to be tested on a single size.  */
/* { dg-do compile } */
/* { dg-require-effective-target sync_int_long } */

#include <stddef.h>
#include <stdbool.h>

int i, e, b;
size_t s;
bool x;

int
main ()
{
  __atomic_compare_exchange_n (&i, &e, 1, 0, __ATOMIC_RELAXED, __ATOMIC_SEQ_CST); /* { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger" } */
  __atomic_compare_exchange_n (&i, &e, 1, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELEASE); /* { dg-warning "invalid failure memory" } */
  __atomic_compare_exchange_n (&i, &e, 1, 1, __ATOMIC_SEQ_CST, __ATOMIC_ACQ_REL); /* { dg-warning "invalid failure memory" } */

  __atomic_load_n (&i, __ATOMIC_RELEASE); /* { dg-warning "invalid memory model" } */
  __atomic_load_n (&i, __ATOMIC_ACQ_REL); /* { dg-warning "invalid memory model" } */

  __atomic_store_n (&i, 1, __ATOMIC_ACQUIRE); /* { dg-warning "invalid memory model" } */
  __atomic_store_n (&i, 1, __ATOMIC_CONSUME); /* { dg-warning "invalid memory model" } */
  __atomic_store_n (&i, 1, __ATOMIC_ACQ_REL); /* { dg-warning "invalid memory model" } */

  i = __atomic_always_lock_free (s, NULL); /* { dg-error "non-constant argument" } */

  __atomic_load_n (&i, 44); /* { dg-warning "invalid memory model" } */

  __atomic_clear (&x, __ATOMIC_CONSUME); /* { dg-warning "invalid memory model" } */
  __atomic_clear (&x, __ATOMIC_ACQUIRE); /* { dg-warning "invalid memory model" } */

  __atomic_clear (&x, __ATOMIC_ACQ_REL); /* { dg-warning "invalid memory model" } */

}
