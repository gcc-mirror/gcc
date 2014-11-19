/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <stdint.h>

uint32_t
truncate_int (const unsigned long long value)
{
  if ( value < 0 )
    {
      return 0;
    }
  else if ( value > UINT32_MAX )
    {
      return UINT32_MAX;
    }
  else
    return (uint32_t)value;
}

uint32_t
mul (const unsigned long long x, const unsigned long long y)
{
  uint32_t value = truncate_int (x * y);
  return value;
}

uint32_t *
test(unsigned size, uint32_t *a, uint32_t s)
{
  unsigned i;

  for (i = 0; i < size; i++)
    {
      a[i] = mul (a[i], s);
    }

  return a;
}
