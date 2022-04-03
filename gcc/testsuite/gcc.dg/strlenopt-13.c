/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) void
fn1 (char *p, const char *y, const char *z, size_t *lp)
{
  char *q, *r, *s;
  char buf1[64], buf2[64];
  size_t l[8];
  /* These two strlen calls stay, all strcpy calls are optimized into
     memcpy, all strchr calls optimized away, and most other strlen
     calls too.  */
  l[0] = strlen (y);
  l[1] = strlen (z);
  strcpy (buf1, y);
  strcpy (buf2, z);
  strcpy (p, "abcde");
  q = strchr (p, '\0');
  strcpy (q, "efghi");
  r = strchr (q, '\0');
  strcpy (r, buf1);
  l[2] = strlen (p);
  l[3] = strlen (q);
  l[4] = strlen (r);
  strcpy (r, buf2);
  /* Except for these two calls, strlen (r) before and after the above
     is non-constant, so adding l[4] - l[1] to all previous strlens
     might make the expressions already too complex.  */
  l[5] = strlen (p);
  l[6] = strlen (q);
  /* This one is of course optimized, it is l[1].  */
  l[7] = strlen (r);
  memcpy (lp, l, sizeof l);
}

int
main ()
{
  char buf[64];
  size_t l[8];
  const char *volatile y = "ABCDEFG";
  const char *volatile z = "HIJK";
  memset (buf, '\0', sizeof buf);
  fn1 (buf, y, z, l);
  if (memcmp (buf, "abcdeefghiHIJK", 15) != 0)
    abort ();
  if (l[0] != 7 || l[1] != 4)
    abort ();
  if (l[2] != 17 || l[3] != 12 || l[4] != 7)
    abort ();
  if (l[5] != 14 || l[6] != 9 || l[7] != 4)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 4 "strlen1" } } */
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
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.1. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.5. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;\[\n\r\]*  l.6. = " 1 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "  _\[0-9\]* = strlen \\(\[^\n\r\]*;" 4 "strlen1" { target { no_alignment_constraints } } } } */
