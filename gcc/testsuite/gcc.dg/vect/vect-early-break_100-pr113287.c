/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_long_long } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

__attribute__((noipa)) void
bar (unsigned long long *p)
{
  __builtin_memset (p, 0, 142 * sizeof (unsigned long long));
  p[17] = 0x50000000000ULL;
}

__attribute__((noipa)) int
foo (void)
{
  unsigned long long r[142];
  bar (r);
  unsigned long long v = ((long long) r[0] >> 31);
  if (v + 1 > 1)
    return 1;
  for (unsigned long long i = 1; i <= 140; ++i)
    if (r[i] != v)
      return 1;
  unsigned long long w = r[141];
  if ((unsigned long long) (((long long) (w << 60)) >> 60) != v)
    return 1;
  return 0;
}

int
main ()
{
  check_vect ();

  if (foo () != 1)
    __builtin_abort ();
}
