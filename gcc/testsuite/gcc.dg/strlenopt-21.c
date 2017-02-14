/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

struct S { char *p; size_t l; };

__attribute__((noinline, noclone)) struct S
foo (char *x, int n)
{
  int i;
  char a[64];
  char *p = strchr (x, '\0');
  struct S s;
  /* strcpy here is optimized into memcpy, length computed as p - x + 1.  */
  strcpy (a, x);
  /* strcat here is optimized into memcpy.  */
  strcat (p, "abcd");
  for (i = 0; i < n; i++)
    if ((i % 123) == 53)
      /* strcat here is optimized into strlen and memcpy.  */
      strcat (a, "efg");
  s.p = strdup (a);
  /* The strlen should be optimized here into 4.  */
  s.l = strlen (p);
  return s;
}

int
main ()
{
  char buf[32];
  struct S s;
  buf[0] = 'z';
  buf[1] = '\0';
  s = foo (buf, 0);
  if (s.l != 4 || memcmp (buf, "zabcd", 6) != 0)
    abort ();
  if (s.p == NULL)
    return 0;
  if (memcmp (s.p, "z", 2) != 0)
    abort ();
  s = foo (buf, 60);
  if (s.l != 4 || memcmp (buf, "zabcdabcd", 10) != 0)
    abort ();
  if (s.p == NULL)
    return 0;
  if (memcmp (s.p, "zabcdefg", 9) != 0)
    abort ();
  s = foo (buf, 240);
  if (s.l != 4 || memcmp (buf, "zabcdabcdabcd", 14) != 0)
    abort ();
  if (s.p == NULL)
    return 0;
  if (memcmp (s.p, "zabcdabcdefgefg", 16) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
