/* Test atomic_load routines for existence and proper execution on
   4-byte values with each valid memory model.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

_Atomic int v;
int count;

int
main ()
{
  v = 0;
  count = 0;

  if (atomic_load_explicit (&v, memory_order_relaxed) != count++)
    abort ();
  else
    v++;

  if (atomic_load_explicit (&v, memory_order_acquire) != count++)
    abort ();
  else
    v++;

  if (atomic_load_explicit (&v, memory_order_consume) != count++)
    abort ();
  else
    v++;

  if (atomic_load_explicit (&v, memory_order_seq_cst) != count++)
    abort ();
  else
    v++;

  if (atomic_load (&v) != count)
    abort ();

  return 0;
}

