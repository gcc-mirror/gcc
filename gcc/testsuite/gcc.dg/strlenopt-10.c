/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) size_t
fn1 (char *p)
{
  char *q;
  /* This can be optimized into memcpy and the size can be decreased to one,
     as it is immediately overwritten.  */
  strcpy (p, "z");
  q = strchr (p, '\0');
  *q = 32;
  /* This strlen can't be optimized away, string length is unknown here.  */
  return strlen (p);
}

__attribute__((noinline, noclone)) void
fn2 (char *p, const char *z, size_t *lp)
{
  char *q, *r;
  char buf[64];
  size_t l[10];
  /* The first strlen stays, all the strcpy calls can be optimized
     into memcpy and all other strlen calls and all strchr calls
     optimized away.  */
  l[0] = strlen (z);
  strcpy (buf, z);
  strcpy (p, "abcde");
  q = strchr (p, '\0');
  strcpy (q, "efghi");
  r = strchr (q, '\0');
  strcpy (r, "jkl");
  l[1] = strlen (p);
  l[2] = strlen (q);
  l[3] = strlen (r);
  strcpy (r, buf);
  l[4] = strlen (p);
  l[5] = strlen (q);
  l[6] = strlen (r);
  strcpy (r, "mnopqr");
  l[7] = strlen (p);
  l[8] = strlen (q);
  l[9] = strlen (r);
  memcpy (lp, l, sizeof l);
}

int
main ()
{
  char buf[64];
  size_t l[10];
  const char *volatile z = "ABCDEFG";
  memset (buf, '\0', sizeof buf);
  if (fn1 (buf) != 2 || buf[0] != 'z' || buf[1] != 32 || buf[2] != '\0')
    abort ();
  fn2 (buf, z, l);
  if (memcmp (buf, "abcdeefghimnopqr", 17) != 0)
    abort ();
  if (l[0] != 7)
    abort ();
  if (l[1] != 13 || l[2] != 8 || l[3] != 3)
    abort ();
  if (l[4] != 17 || l[5] != 12 || l[6] != 7)
    abort ();
  if (l[7] != 16 || l[8] != 11 || l[9] != 6)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen1" } } */
/* Some targets have BIGGEST_ALIGNMENT 8-bits, allowing fold_builtin_memory_op
   to expand the memcpy call at the end of fn2.  */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 8 "strlen1" { target { ! no_alignment_constraints } } } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 7 "strlen1" { target { no_alignment_constraints} } } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "\\*q_\[0-9\]* = 32;" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(\[^\n\r\]*, 1\\)" 1 "strlen1" } } */
