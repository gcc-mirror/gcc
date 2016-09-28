/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) const char *
fn1 (int x, int y)
{
  const char *p;
  switch (x)
    {
    case 0:
      p = "abcd";
      /* Prevent cswitch optimization.  */
      asm volatile ("" : : : "memory");
      break;
    case 1:
      p = "efgh";
      break;
    case 2:
      p = "ijkl";
      break;
    default:
      p = "mnop";
      break;
    }
  if (y)
    /* strchr should be optimized into p + 4 here.  */
    return strchr (p, '\0');
  else
    /* and strlen into 3.  */
    return p + strlen (p + 1);
}

__attribute__((noinline, noclone)) size_t
fn2 (char *p, char *q)
{
  size_t l;
  /* Both strcpy calls can be optimized into memcpy, strlen needs to stay.  */
  strcpy (p, "abc");
  p[3] = 'd';
  l = strlen (p);
  strcpy (q, p);
  return l;
}

__attribute__((noinline, noclone)) char *
fn3 (char *p)
{
  char *c;
  /* The strcpy call can be optimized into memcpy, strchr needs to stay,
     strcat is optimized into memcpy.  */
  strcpy (p, "abc");
  p[3] = 'd';
  c = strchr (p, '\0');
  strcat (p, "efgh");
  return c;
}

int
main ()
{
  int i;
  char buf[64], buf2[64];
  for (i = 0; i < 5; i++)
    {
      const char *p = "abcdefghijklmnop" + (i < 3 ? i : 3) * 4;
      const char *q;
      q = fn1 (i, 1);
      if (memcmp (q - 4, p, 4) != 0 || q[0] != '\0')
	abort ();
      q = fn1 (i, 0);
      if (memcmp (q - 3, p, 4) != 0 || q[1] != '\0')
	abort ();
    }
  memset (buf, '\0', sizeof buf);
  memset (buf + 4, 'z', 2);
  if (fn2 (buf, buf2) != 6
      || memcmp (buf, "abcdzz", 7) != 0
      || memcmp (buf2, "abcdzz", 7) != 0)
    abort ();
  memset (buf, '\0', sizeof buf);
  memset (buf + 4, 'z', 2);
  if (fn3 (buf) != buf + 6 || memcmp (buf, "abcdzzefgh", 11) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 4 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
