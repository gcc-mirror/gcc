/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
foo (char *p, char *r)
{
  char buf[26];
  if (strlen (p) + strlen (r) + 9 > 26)
    return NULL;
  /* This strcpy can be optimized into memcpy, using the remembered
     strlen (p).  */
  strcpy (buf, p);
  /* These two strcat can be optimized into memcpy.  The first one
     could be even optimized into a *ptr = '/'; store as the '\0'
     is immediately overwritten.  */
  strcat (buf, "/");
  strcat (buf, "abcde");
  /* This strcpy can be optimized into memcpy, using the remembered
     strlen (r).  */
  strcat (buf, r);
  /* And this can be optimized into memcpy too.  */
  strcat (buf, "fg");
  return strdup (buf);
}

int
main ()
{
  char *volatile p = "string1";
  char *volatile r = "string2";
  char *q = foo (p, r);
  if (q != NULL)
    {
      if (strcmp (q, "string1/abcdestring2fg"))
	abort ();
      free (q);
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 5 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
