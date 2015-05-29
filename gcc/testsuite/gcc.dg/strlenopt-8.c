/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

/* Yes, there are people who write code like this.  */

__attribute__((noinline, noclone)) char *
foo (int r)
{
  char buf[10] = "";
  strcat (buf, r ? "r" : "w");
  strcat (buf, "b");
  return strdup (buf);
}

__attribute__((noinline, noclone)) char *
bar (int r)
{
  char buf[10] = {};
  strcat (buf, r ? "r" : "w");
  strcat (buf, "b");
  return strdup (buf);
}

int
main ()
{
  char *q = foo (1);
  if (q != NULL)
    {
      if (strcmp (q, "rb"))
	abort ();
      free (q);
    }
  q = bar (0);
  if (q != NULL)
    {
      if (strcmp (q, "wb"))
	abort ();
      free (q);
    }
  return 0;
}

/* On non-strict-align targets we inline the memcpy that strcat is turned
   into and end up with a short typed load / store which strlenopt is not
   able to analyze.  */

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" { xfail non_strict_align } } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 2 "strlen" { target { non_strict_align } } } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 4 "strlen" { target { ! non_strict_align } } } }  */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
