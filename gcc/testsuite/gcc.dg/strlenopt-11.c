/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) void
fn1 (char *p, const char *z, size_t *lp)
{
  char *q, *r, *s;
  char buf[64];
  size_t l[11];
  /* The first strlen stays, all the strcpy calls can be optimized
     into memcpy and most other strlen calls and all strchr calls
     optimized away.  l[6] = strlen (r); and l[9] = strlen (r); need
     to stay, because we need to invalidate the knowledge about
     r strlen after strcpy (q, "jklmnopqrst").  */
  l[0] = strlen (z);
  strcpy (buf, z);
  strcpy (p, "abcde");
  q = strchr (p, '\0');
  strcpy (q, "efghi");
  r = strchr (q, '\0');
  strcpy (r, buf);
  l[1] = strlen (p);
  l[2] = strlen (q);
  l[3] = strlen (r);
  strcpy (q, "jklmnopqrst");
  l[4] = strlen (p);
  l[5] = strlen (q);
  l[6] = strlen (r);
  s = strchr (q, '\0');
  strcpy (s, buf);
  l[7] = strlen (p);
  l[8] = strlen (q);
  l[9] = strlen (r);
  l[10] = strlen (s);
  memcpy (lp, l, sizeof l);
}

int
main ()
{
  char buf[64];
  size_t l[11];
  const char *volatile z = "ABCDEFG";
  memset (buf, '\0', sizeof buf);
  fn1 (buf, z, l);
  if (memcmp (buf, "abcdejklmnopqrstABCDEFG", 24) != 0)
    abort ();
  if (l[0] != 7)
    abort ();
  if (l[1] != 17 || l[2] != 12 || l[3] != 7)
    abort ();
  if (l[4] != 16 || l[5] != 11 || l[6] != 6)
    abort ();
  if (l[7] != 23 || l[8] != 18 || l[9] != 13 || l[10] != 7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 3 "strlen1" } } */
/* Some targets have BIGGEST_ALIGNMENT 8-bits, allowing fold_builtin_memory_op
   to expand the memcpy call at the end of fn1.  */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 7 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 6 "strlen1" { target { no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen1" } } */
/* Where the memcpy is expanded, the assignemts to elements of l are
   propagated.  */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.0. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.6. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.9. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;" 3 "strlen1" { target { no_alignment_constraints } } } } */
