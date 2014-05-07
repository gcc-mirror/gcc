/* PR target/57623 */
/* { dg-do assemble { target bmi2 } } */
/* { dg-options "-O2 -mbmi2" } */

#include <x86intrin.h>

unsigned int
f1 (unsigned int x, unsigned int *y)
{
  return _bzhi_u32 (x, *y);
}

unsigned int
f2 (unsigned int *x, unsigned int y)
{
  return _bzhi_u32 (*x, y);
}

#ifdef  __x86_64__
unsigned long long
f3 (unsigned long long x, unsigned long long *y)
{
  return _bzhi_u64 (x, *y);
}

unsigned long long
f4 (unsigned long long *x, unsigned long long y)
{
  return _bzhi_u64 (*x, y);
}
#endif
