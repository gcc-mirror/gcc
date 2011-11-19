/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) size_t
bar (char *p, char *q)
{
  size_t l1, l2, l3;
  char *r = strchr (p, '\0');
  strcpy (r, "abcde");
  char *s = strchr (r, '\0');
  strcpy (s, q);
  l1 = strlen (p);
  l2 = strlen (r);
  l3 = strlen (s);
  return l1 + l2 + l3;
}

int
main ()
{
  char buf[16] = "01234";

  if (bar (buf, "56789") != 30)
    abort ();

  if (memcmp (buf, "01234abcde56789", 16) != 0)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } *
/* { dg-final { cleanup-tree-dump "strlen" } } */
