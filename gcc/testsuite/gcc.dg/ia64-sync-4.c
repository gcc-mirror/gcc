/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O2 -finline-functions" } */

/* Test inlining __sync_bool_compare_and_swap_di.  */

#include <stdbool.h>
#include <ia64intrin.h>

static bool
compare_and_swap(long *addr, long old, long new_val)
{
  return __sync_bool_compare_and_swap_di(addr, old, new_val);
}

void
foo (long *address)
{
  long he_address = *address & ~1;
  while (!compare_and_swap(address, he_address, he_address | 1))
    he_address = *address & ~1;
}
