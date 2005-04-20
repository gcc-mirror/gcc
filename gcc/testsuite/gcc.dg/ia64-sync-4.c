/* { dg-do compile } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options "-O2 -finline-functions" } */
/* { dg-options "-march=i486" { target i?86-*-* } } */
/* { dg-options "-march=i486" { target { x86_64-*-* && ilp32 } } } */

/* Test inlining __sync_bool_compare_and_swap.  */

#include <stdbool.h>

static bool
compare_and_swap(long *addr, long old, long new_val)
{
  return __sync_bool_compare_and_swap(addr, old, new_val);
}

void
foo (long *address)
{
  long he_address = *address & ~1;
  while (!compare_and_swap(address, he_address, he_address | 1))
    he_address = *address & ~1;
}
