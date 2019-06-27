/* PRE tree-optimization/90626 - fold strcmp(a, b) == 0 to zero when
   one string length is exact and the other is unequal
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))


__attribute__ ((noclone, noinline, noipa)) void
clobber (void *p, int x, size_t n)
{
  for (volatile unsigned char *q = p; n--; )
    *q = x;
}

__attribute__ ((noclone, noinline, noipa)) void
test_strcmp (void)
{
  char a[8], b[8];
  strcpy (a, "1235");
  strcpy (b, "1234");

  A (strcmp (a, b));

  clobber (a, 0, sizeof a);
  clobber (b, 0, sizeof b);
  clobber (b + 4, '5', 1);

  memcpy (a, "1234", 4);
  memcpy (b, "1234", 4);

  A (0 > strcmp (a, b));
  A (0 < strcmp (b, a));
}

__attribute__ ((noclone, noinline, noipa)) void
test_strncmp (void)
{
  char a[8], b[8];
  strcpy (a, "1235");
  strcpy (b, "1234");

  A (0 == strncmp (a, b, 1));
  A (0 == strncmp (a, b, 2));
  A (0 == strncmp (a, b, 3));
  A (0 <  strncmp (a, b, 4));
  A (0 >  strncmp (b, a, 4));

  clobber (a, 0, sizeof a);
  clobber (b, 0, sizeof b);
  clobber (b + 4, '5', 1);

  memcpy (a, "1234", 4);
  memcpy (b, "1234", 4);

  A (0 == strncmp (a, b, 4));
  A (0 >  strncmp (a, b, 5));
  A (0 <  strncmp (b, a, 5));
}

int main (void)
{
  test_strcmp ();
  test_strncmp ();
}
