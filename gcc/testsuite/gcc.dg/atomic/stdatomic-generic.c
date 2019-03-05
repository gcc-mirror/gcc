/* Test generic atomic routines for proper function calling.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);

typedef struct test {
  int array[10];
} test_struct;

test_struct zero = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
test_struct ones = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
_Atomic test_struct a;
test_struct b;

int size = sizeof (test_struct);
/* Test for consistency on sizes 1, 2, 4, 8, 16 and 32.  */
int
main ()
{
  test_struct c;

  atomic_store_explicit (&a, zero, memory_order_relaxed);
  if (memcmp (&a, &zero, size))
    abort ();

  c = atomic_exchange_explicit (&a, ones, memory_order_seq_cst);
  if (memcmp (&c, &zero, size))
    abort ();
  if (memcmp (&a, &ones, size))
    abort ();

  b = atomic_load_explicit (&a, memory_order_relaxed);
  if (memcmp (&b, &ones, size))
    abort ();

  if (!atomic_compare_exchange_strong_explicit (&a, &b, zero, memory_order_seq_cst, memory_order_acquire))
    abort ();
  if (memcmp (&a, &zero, size))
    abort ();

  if (atomic_compare_exchange_weak_explicit (&a, &b, ones, memory_order_seq_cst, memory_order_acquire))
    abort ();
  if (memcmp (&b, &zero, size))
    abort ();

  return 0;
}

