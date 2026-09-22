/* { dg-do compile } */
/* { dg-options "-march=armv8-a -O2" } */

#include <stdatomic.h>

void
foo (int *p, long *q)
{
  atomic_store_explicit (p, 2, memory_order_relaxed);
  atomic_store_explicit (p + 1, 2, memory_order_release);
  atomic_store_explicit (p + 2, 2, memory_order_relaxed);
}

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 2" 1 } } */
