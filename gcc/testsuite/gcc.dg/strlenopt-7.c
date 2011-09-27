/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen -fdump-tree-optimized" } */

#include "strlenopt.h"

char buf[64];

__attribute__((noinline, noclone)) size_t
foo (void)
{
  char *p = memcpy (buf, "abcdefgh", 9);
  /* This store can be optimized away as... */
  *p = '\0';
  /* ... the following strcat can be optimized into memcpy,
     which overwrites that '\0'.  */
  strcat (p, "ijk");
  /* This should be optimized into return 3.  */
  return strlen (p);
}

__attribute__((noinline, noclone)) size_t
bar (char *p)
{
  char *r = strchr (p, '\0');
  /* This store shouldn't be optimized away, because we
     want to crash if p is e.g. a string literal.  */
  *r = '\0';
  /* This strlen can be optimized into 0.  */
  return strlen (r);
}

int
main ()
{
  char *volatile p = buf;
  if (foo () != 3 || memcmp (buf, "ijk\0efgh\0", 10) != 0)
    abort ();
  if (bar (p) != 0 || memcmp (buf, "ijk\0efgh\0", 10) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "\\*r_\[0-9\]* = 0;" 1 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
/* { dg-final { scan-tree-dump-times "return 3;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
