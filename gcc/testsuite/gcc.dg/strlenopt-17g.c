/* This test needs runtime that provides stpcpy function.  */
/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt.h"

__attribute__((noinline, noclone)) int
foo (const char *p)
{
  static int c;
  const char *q[] = { "123498765abcde", "123498765..", "129abcde", "129abcde" };
  if (strcmp (p, q[c]) != 0)
    abort ();
  return c++;
}

__attribute__((noinline, noclone)) void
bar (const char *p, const char *q)
{
  size_t l;
  /* This strlen stays.  */
  char *a = __builtin_alloca (strlen (p) + 50);
  /* strcpy can be optimized into memcpy.  */
  strcpy (a, p);
  /* strcat into stpcpy.  */
  strcat (a, q);
  /* This strlen can be optimized away.  */
  l = strlen (a);
  /* This becomes memcpy.  */
  strcat (a, "abcde");
  if (!foo (a))
    /* And this one too.  */
    strcpy (a + l, "..");
  foo (a);
}

int
main ()
{
  const char *volatile s1 = "1234";
  const char *volatile s2 = "98765";
  const char *volatile s3 = "12";
  const char *volatile s4 = "9";
  bar (s1, s2);
  bar (s3, s4);
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 3 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "mempcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 1 "strlen1" } } */
