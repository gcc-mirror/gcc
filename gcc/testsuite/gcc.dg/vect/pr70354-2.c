/* PR tree-optimization/70354 */

#ifndef main
#include "tree-vect.h"
#endif

unsigned long long a[64], b[64];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < 64; i++)
    a[i] <<= (b[i] - 0x1200000000ULL);
}

int
main ()
{
  int i;
#ifndef main
  check_vect ();
#endif
  if (__CHAR_BIT__ != 8 || sizeof (long long int) != 8)
    return 0;
  for (i = 0; i < 64; i++)
    {
      a[i] = 0x1234ULL;
      b[i] = 0x1200000000ULL + (i % 54);
    }
  foo ();
  for (i = 0; i < 64; i++)
    if (a[i] != (0x1234ULL << (i % 54)))
      abort ();
  return 0;
}
