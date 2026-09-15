/* { dg-require-effective-target bitint } */

#include <stdint.h>
#include "tree-vect.h"

void __attribute__((noipa))
foo (int64_t *a, signed _BitInt(17) *b)
{
  for (int i = 0; i < 32; i++)
    a[i] = b[i];
}

int
main ()
{
  int64_t a[32];
  _BitInt(17) b[32];

  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 32; ++i)
    b[i] = i;
  /* Now set all padding bits in b[] to 1.  */
  _BitInt(17) z = ~0;
  __builtin_clear_padding (&z);
#pragma GCC novector
  for (int i = 0; i < 32; ++i)
    for (int j = 0; j < sizeof (z); ++j)
      ((char *)&b[i])[j] |= ~(((char *)&z)[j]);

  foo (a, b);

#pragma GCC novector
  for (int i = 0; i < 32; ++i)
    if (a[i] != i)
      __builtin_abort ();

  return 0;
}
