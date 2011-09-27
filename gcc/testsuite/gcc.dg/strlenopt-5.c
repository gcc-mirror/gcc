/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
foo (char *p, const char *q)
{
  char *e = strchr (p, '\0');
  strcat (p, q);
  return e;
}

__attribute__((noinline, noclone)) char *
bar (char *p)
{
  memcpy (p, "abcd", 5);
  return strchr (p, '\0');
}

__attribute__((noinline, noclone)) void
baz (char *p)
{
  char *e = strchr (p, '\0');
  strcat (e, "abcd");
}

char buf[64];

int
main ()
{
  char *volatile p = buf;
  const char *volatile q = "ij";
  memset (buf, 'v', 3);
  if (foo (p, q) != buf + 3
      || memcmp (buf, "vvvij\0\0\0\0", 10) != 0)
    abort ();
  memset (buf, '\0', sizeof buf);
  if (bar (p) != buf + 4
      || memcmp (buf, "abcd\0\0\0\0\0", 10) != 0)
    abort ();
  memset (buf, 'v', 2);
  memset (buf + 2, '\0', -2 + sizeof buf);
  baz (p);
  if (memcmp (buf, "vvabcd\0\0\0", 10) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
