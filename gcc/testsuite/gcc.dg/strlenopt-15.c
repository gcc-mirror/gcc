/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) size_t
fn1 (char *p, size_t l)
{
  memcpy (p, "abcdef", l);
  /* This strlen can't be optimized, as l is unknown.  */
  return strlen (p);
}

__attribute__((noinline, noclone)) size_t
fn2 (char *p, const char *q, size_t *lp)
{
  size_t l = strlen (q), l2;
  memcpy (p, q, 7);
  /* This strlen can't be optimized, as l might be bigger than 7.  */
  l2 = strlen (p);
  *lp = l;
  return l2;
}

__attribute__((noinline, noclone)) char *
fn3 (char *p)
{
  *p = 0;
  return p + 1;
}

int
main ()
{
  char buf[64];
  const char *volatile q = "ABCDEFGH";
  const char *volatile q2 = "IJ\0KLMNOPQRS";
  size_t l;
  memset (buf, '\0', sizeof buf);
  memset (buf + 2, 'a', 7);
  if (fn1 (buf, 3) != 9 || memcmp (buf, "abcaaaaaa", 10) != 0)
    abort ();
  if (fn1 (buf, 7) != 6 || memcmp (buf, "abcdef\0aa", 10) != 0)
    abort ();
  if (fn2 (buf, q, &l) != 9 || l != 8 || memcmp (buf, "ABCDEFGaa", 10) != 0)
    abort ();
  if (fn2 (buf, q2, &l) != 2 || l != 2 || memcmp (buf, "IJ\0KLMNaa", 10) != 0)
    abort ();
  if (fn3 (buf) != buf + 1 || memcmp (buf, "\0J\0KLMNaa", 10) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
