/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

/* If stpcpy can't be used, this is optimized into
   strcpy (p, q); strcat (p, r); memcpy (p + strlen (p), "abcd", 5);
   If stpcpy can be used (see strlenopt-4g.c test),
   this is optimized into
   memcpy (stpcpy (stpcpy (p, q), r), "abcd", 5);  */
__attribute__((noinline, noclone)) void
foo (char *p, const char *q, const char *r)
{
  strcpy (p, q);
  strcat (p, r);
  strcat (p, "abcd");
}

/* If stpcpy can't be used, this is optimized into
   memcpy (p, "abcd", 4); strcpy (p + 4, q); strcat (p, r);
   If stpcpy can be used, this is optimized into
   memcpy (p, "abcd", 4); strcpy (stpcpy (p + 4, q), r);  */
__attribute__((noinline, noclone)) void
bar (char *p, const char *q, const char *r)
{
  strcpy (p, "abcd");
  strcat (p, q);
  strcat (p, r);
}

/* If stpcpy can't be used, this is optimized into
   strcat (p, q); memcpy (t1 = p + strlen (p), "abcd", 4);
   strcpy (t1 + 4, r); memcpy (p + strlen (p), "efgh", 5);
   If stpcpy can be used, this is optimized into
   t1 = stpcpy (p + strlen (p), q); memcpy (t1, "abcd", 4);
   memcpy (stpcpy (t1 + 4, r), "efgh", 5);  */
__attribute__((noinline, noclone)) void
baz (char *p, const char *q, const char *r)
{
  strcat (p, q);
  strcat (p, "abcd");
  strcat (p, r);
  strcat (p, "efgh");
}

char buf[64];

int
main ()
{
  char *volatile p = buf;
  const char *volatile q = "ij";
  const char *volatile r = "klmno";
  foo (p, q, r);
  if (memcmp (buf, "ijklmnoabcd\0\0\0\0\0\0\0\0", 20) != 0)
    abort ();
  memset (buf, '\0', sizeof buf);
  bar (p, q, r);
  if (memcmp (buf, "abcdijklmno\0\0\0\0\0\0\0\0", 20) != 0)
    abort ();
  memset (buf, 'v', 3);
  memset (buf + 3, '\0', -3 + sizeof buf);
  baz (p, q, r);
  if (memcmp (buf, "vvvijabcdklmnoefgh\0", 20) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 4 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 3 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
