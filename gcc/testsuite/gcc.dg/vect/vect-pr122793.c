/* { dg-additional-options "-msse4" { target sse4_runtime } } */

#include "tree-vect.h"

static void
foo (unsigned char *d, unsigned char *s, int e, int f)
{
  for (int i = 0; i < 4; i++)
    {
      d[0] = s[-2];
      d[5] = (s[5] + s[6]) * 2 - (s[4] + s[7]);
      d[6] = (s[6] + s[7]) * 2 - (s[5] + s[8]);
      d[7] = (s[7] + s[8]) * 2 - (s[6] + s[9]);
      d += e;
      s += f;
    }
}

unsigned char s[128] = { 2 }, d[128];

int
main ()
{
  check_vect ();
  foo (d, s + 2, 16, 16);
  if (d[5] != 0)
    __builtin_abort ();
}
