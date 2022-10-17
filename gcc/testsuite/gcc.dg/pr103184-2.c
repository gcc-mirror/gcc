/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>

int
tbit0 (_Atomic int* a, int n)
{
#define BIT (0x1 << n)
  return atomic_fetch_or (a, BIT) & BIT;
#undef BIT
}
