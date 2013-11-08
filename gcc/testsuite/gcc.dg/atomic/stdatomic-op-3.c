/* Test atomic_fetch routines for existence and proper execution on
   4-byte values with each valid memory model.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

_Atomic int v;
int count, res;
const int init = ~0;

void
test_fetch_add ()
{
  v = 0;
  count = 1;

  if (atomic_fetch_add_explicit (&v, count, memory_order_relaxed) != 0)
    abort ();

  if (atomic_fetch_add_explicit (&v, 1, memory_order_consume) != 1)
    abort ();

  if (atomic_fetch_add_explicit (&v, count, memory_order_acquire) != 2)
    abort ();

  if (atomic_fetch_add_explicit (&v, 1, memory_order_release) != 3)
    abort ();

  if (atomic_fetch_add_explicit (&v, count, memory_order_acq_rel) != 4)
    abort ();

  if (atomic_fetch_add_explicit (&v, 1, memory_order_seq_cst) != 5)
    abort ();

  if (atomic_fetch_add (&v, 1) != 6)
    abort ();
}

void
test_fetch_sub ()
{
  v = res = 20;
  count = 0;

  if (atomic_fetch_sub_explicit (&v, count + 1, memory_order_relaxed) != res--)
    abort ();

  if (atomic_fetch_sub_explicit (&v, 1, memory_order_consume) != res--)
    abort ();

  if (atomic_fetch_sub_explicit (&v, count + 1, memory_order_acquire) != res--)
    abort ();

  if (atomic_fetch_sub_explicit (&v, 1, memory_order_release) != res--)
    abort ();

  if (atomic_fetch_sub_explicit (&v, count + 1, memory_order_acq_rel) != res--)
    abort ();

  if (atomic_fetch_sub_explicit (&v, 1, memory_order_seq_cst) != res--)
    abort ();

  if (atomic_fetch_sub (&v, 1) != res--)
    abort ();
}

void
test_fetch_and ()
{
  v = init;

  if (atomic_fetch_and_explicit (&v, 0, memory_order_relaxed) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, init, memory_order_consume) != 0)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_acquire) != 0)
    abort ();

  v = ~v;
  if (atomic_fetch_and_explicit (&v, init, memory_order_release) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_acq_rel) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_seq_cst) != 0)
    abort ();

  if (atomic_fetch_and (&v, 0) != 0)
    abort ();
}

void
test_fetch_xor ()
{
  v = init;
  count = 0;

  if (atomic_fetch_xor_explicit (&v, count, memory_order_relaxed) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_consume) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, 0, memory_order_acquire) != 0)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_release) != 0)
    abort ();

  if (atomic_fetch_xor_explicit (&v, 0, memory_order_acq_rel) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_seq_cst) != init)
    abort ();

  if (atomic_fetch_xor (&v, ~count) != 0)
    abort ();
}

void
test_fetch_or ()
{
  v = 0;
  count = 1;

  if (atomic_fetch_or_explicit (&v, count, memory_order_relaxed) != 0)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, 2, memory_order_consume) != 1)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_acquire) != 3)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, 8, memory_order_release) != 7)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_acq_rel) != 15)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_seq_cst) != 31)
    abort ();

  count *= 2;
  if (atomic_fetch_or (&v, count) != 63)
    abort ();
}


/* Test the OP routines with a result which isn't used.  */

void
test_add ()
{
  v = 0;
  count = 1;

  atomic_fetch_add (&v, count);
  if (v != 1)
    abort ();

  atomic_fetch_add_explicit (&v, count, memory_order_consume);
  if (v != 2)
    abort ();

  atomic_fetch_add (&v, 1);
  if (v != 3)
    abort ();

  atomic_fetch_add_explicit (&v, 1, memory_order_release);
  if (v != 4)
    abort ();

  atomic_fetch_add (&v, 1);
  if (v != 5)
    abort ();

  atomic_fetch_add_explicit (&v, count, memory_order_seq_cst);
  if (v != 6)
    abort ();
}

void
test_sub ()
{
  v = res = 20;
  count = 0;

  atomic_fetch_sub (&v, count + 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, count + 1, memory_order_consume);
  if (v != --res)
    abort ();

  atomic_fetch_sub (&v, 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, 1, memory_order_release);
  if (v != --res)
    abort ();

  atomic_fetch_sub (&v, count + 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, count + 1, memory_order_seq_cst);
  if (v != --res)
    abort ();
}

void
test_and ()
{
  v = init;

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = init;
  atomic_fetch_and_explicit (&v, init, memory_order_consume);
  if (v != init)
    abort ();

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = ~v;
  atomic_fetch_and_explicit (&v, init, memory_order_release);
  if (v != init)
    abort ();

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = ~v;
  atomic_fetch_and_explicit (&v, 0, memory_order_seq_cst);
  if (v != 0)
    abort ();
}

void
test_xor ()
{
  v = init;
  count = 0;

  atomic_fetch_xor (&v, count);
  if (v != init)
    abort ();

  atomic_fetch_xor_explicit (&v, ~count, memory_order_consume);
  if (v != 0)
    abort ();

  atomic_fetch_xor (&v, 0);
  if (v != 0)
    abort ();

  atomic_fetch_xor_explicit (&v, ~count, memory_order_release);
  if (v != init)
    abort ();

  atomic_fetch_xor_explicit (&v, 0, memory_order_acq_rel);
  if (v != init)
    abort ();

  atomic_fetch_xor (&v, ~count);
  if (v != 0)
    abort ();
}

void
test_or ()
{
  v = 0;
  count = 1;

  atomic_fetch_or (&v, count);
  if (v != 1)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, count, memory_order_consume);
  if (v != 3)
    abort ();

  count *= 2;
  atomic_fetch_or (&v, 4);
  if (v != 7)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, 8, memory_order_release);
  if (v != 15)
    abort ();

  count *= 2;
  atomic_fetch_or (&v, count);
  if (v != 31)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, count, memory_order_seq_cst);
  if (v != 63)
    abort ();
}

int
main ()
{
  test_fetch_add ();
  test_fetch_sub ();
  test_fetch_and ();
  test_fetch_xor ();
  test_fetch_or ();

  test_add ();
  test_sub ();
  test_and ();
  test_xor ();
  test_or ();

  return 0;
}
