/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt.h"

volatile int v;

size_t __attribute__ ((noinline, noclone))
f1 (char *b)
{
  char a[30];
  v += 1;
  // Should be converted to stpcpy.
  strcpy (a, b);
  int len1 = strlen (a);
  a[0] = '_';
  a[1] = 0;
  return len1 + strlen (a);
}

size_t __attribute__ ((noinline, noclone))
f2 (char *a, char *b)
{
  v += 2;
  // Should be converted to stpcpy.
  strcpy (a, b);
  int len1 = strlen (a);
  a[0] = '_';
  a[1] = 0;
  return len1 + strlen (a);
}

int
main ()
{
  char a[30];
  if (f1 ("foo") != 4 || f2 (a, "foobar") != 7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 2 "strlen1" } } */
