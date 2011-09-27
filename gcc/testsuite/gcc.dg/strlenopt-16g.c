/* This test needs runtime that provides stpcpy function.  */
/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
fn1 (char *p, const char *q)
{
  /* This strcpy can be optimized into stpcpy.  */
  strcpy (p, q);
  /* And this strchr into the return value from it.  */
  return strchr (p, '\0');
}

int
main ()
{
  char buf[64];
  const char *volatile q = "ABCDEFGH";
  if (fn1 (buf, q) != buf + 8 || memcmp (buf, "ABCDEFGH", 9) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "mempcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 1 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
