/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
foo (char *p, char *r)
{
  char *q = malloc (strlen (p) + strlen (r) + 64);
  if (q == NULL) return NULL;
  /* This strcpy can be optimized into memcpy, using the remembered
     strlen (p).  */
  strcpy (q, p);
  /* These two strcat can be optimized into memcpy.  The first one
     could be even optimized into a *ptr = '/'; store as the '\0'
     is immediately overwritten.  */
  strcat (q, "/");
  strcat (q, "abcde");
  /* Due to inefficient PTA (PR50262) the above calls invalidate
     string length of r, so it is optimized just into strcpy instead
     of memcpy.  */
  strcat (q, r);
  return q;
}

int
main ()
{
  char *volatile p = "string1";
  char *volatile r = "string2";
  char *q = foo (p, r);
  if (q != NULL)
    {
      if (strcmp (q, "string1/abcdestring2"))
	abort ();
      free (q);
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
