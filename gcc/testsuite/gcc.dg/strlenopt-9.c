/* { dg-do run } */
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-strlen -fdump-tree-optimized" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
fn1 (int r)
{
  char *p = r ? "a" : "bc";
  /* String length for p varies, therefore strchr below isn't
     optimized away.  */
  return strchr (p, '\0');
}

__attribute__((noinline, noclone)) size_t
fn2 (int r)
{
  char *p, q[10];
  strcpy (q, "abc");
  p = r ? "a" : q;
  /* String length is constant for both alternatives, and strlen is
     optimized away.  */
  return strlen (p);
}

__attribute__((noinline, noclone)) size_t
fn3 (char *p, int n)
{
  int i;
  p = strchr (p, '\0');
  /* strcat here can be optimized into memcpy.  */
  strcat (p, "abcd");
  for (i = 0; i < n; i++)
    if ((i % 123) == 53)
      /* strcat here is optimized into strlen and memcpy.  */
      strcat (p, "efg");
  /* The strlen here can't be optimized away, as in the loop string
     length of p might change.  */
  return strlen (p);
}

char buf[64];

__attribute__((noinline, noclone)) size_t
fn4 (char *x, int n)
{
  int i;
  size_t l;
  char a[64];
  char *p = strchr (x, '\0');
  /* strcpy here is optimized into memcpy, length computed as p - x + 1.  */
  strcpy (a, x);
  /* strcat here is optimized into memcpy.  */
  strcat (p, "abcd");
  for (i = 0; i < n; i++)
    if ((i % 123) == 53)
      /* strcat here is optimized into strlen and memcpy.  */
      strcat (a, "efg");
  /* The strlen should be optimized here into 4.  */
  l = strlen (p);
  /* This stays strcpy.  */
  strcpy (buf, a);
  return l;
}

int
main ()
{
  volatile int l = 1;
  char b[64];

  if (memcmp (fn1 (l) - 1, "a", 2) != 0)
    abort ();
  if (memcmp (fn1 (!l) - 2, "bc", 3) != 0)
    abort ();
  if (fn2 (l) != 1 || fn2 (!l) != 3)
    abort ();
  memset (b, '\0', sizeof b);
  memset (b, 'a', 3);
  if (fn3 (b, 10) != 4 || memcmp (b, "aaaabcd", 8) != 0)
    abort ();
  if (fn3 (b, 128) != 7 || memcmp (b, "aaaabcdabcdefg", 15) != 0)
    abort ();
  if (fn3 (b, 256) != 10 || memcmp (b, "aaaabcdabcdefgabcdefgefg", 25) != 0)
    abort ();
  if (fn4 (b, 10) != 4
      || memcmp (b, "aaaabcdabcdefgabcdefgefgabcd", 29) != 0
      || memcmp (buf, "aaaabcdabcdefgabcdefgefg", 25) != 0)
    abort ();
  if (fn4 (b, 128) != 4
      || memcmp (b, "aaaabcdabcdefgabcdefgefgabcdabcd", 33) != 0
      || memcmp (buf, "aaaabcdabcdefgabcdefgefgabcdefg", 32) != 0)
    abort ();
  if (fn4 (b, 256) != 4
      || memcmp (b, "aaaabcdabcdefgabcdefgefgabcdabcdabcd", 37) != 0
      || memcmp (buf, "aaaabcdabcdefgabcdefgefgabcdabcdefgefg", 39) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 6 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "return 4;" 1 "optimized" } } */
