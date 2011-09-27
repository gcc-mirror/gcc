/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
foo (char *x)
{
#ifdef PR50262_FIXED
  /* Once PTA is fixed, we'll need just one strlen here,
     without the memcpy.  */
  char *p = x;
  char *q = malloc (strlen (p) + 64);
#else
  /* This is here just because PTA can't figure that
     *q = '\0' store below can't change p's length.
     In this case we have one strlen and one memcpy here.  */
  char b[64];
  char *q = malloc (strlen (x) + 64);
  char *p = strcpy (b, x);
#endif
  char *r;
  if (q == NULL) return NULL;
  /* This store can be optimized away once strcat is
     replaced with memcpy.  */
  *q = '\0';
  /* These two strcat calls can be optimized into memcpy calls.  */
  strcat (q, p);
  strcat (q, "/");
  /* The strchr can be optimized away, as we know the current
     string length as well as end pointer.  */
  r = strchr (q, '\0');
  /* This store can go, as it is overwriting '\0' with the same
     character.  */
  *r = '\0';
  /* And this strcat can be again optimized into memcpy call.  */
  strcat (q, "abcde");
  return q;
}

__attribute__((noinline, noclone)) char *
bar (char *p)
{
  char buf[26];
  char *r;
  if (strlen (p) + 9 > 26)
    return NULL;
  *buf = '\0';
  strcat (buf, p);
  strcat (buf, "/");
  r = strchr (buf, '\0');
  *r = '\0';
  strcat (buf, "abcde");
  return strdup (buf);
}

int
main ()
{
  char *volatile p = "string1";
  char *volatile r = "string2";
  char *q = foo (p);
  if (q != NULL)
    {
      if (strcmp (q, "string1/abcde"))
	abort ();
      memset (q, '\0', 14);
      free (q);
    }
  q = bar (p);
  if (q != NULL)
    {
      if (strcmp (q, "string1/abcde"))
	abort ();
      free (q);
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 7 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
