/* PR tree-optimization/83444 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "abort \\(\\)" "optimized" } } */

#include "strlenopt.h"

void
foo (void)
{
  char a[5] = "012";
  strcpy (a, "");
  if (strlen (a) != 0)
    abort ();
}

void
bar (void)
{
  char a[5] = "012";
  char b[7] = "";
  strcpy (a, b);
  if (strlen (a) != 0)
    abort ();
}

struct S { char a[4]; char b[5]; char c[7]; };

void
baz (void)
{
  struct S s;
  strcpy (s.b, "012");
  strcpy (s.c, "");
  strcpy (s.b, s.c);
  if (s.b[0] != 0)
    abort ();
}

void
boo (void)
{
  struct S s;
  strcpy (s.b, "012");
  strcpy (s.c, "");
  strcpy (s.b, s.c);
  if (strlen (s.b) != 0)
    abort ();
}
