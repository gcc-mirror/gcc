/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen -fdump-tree-optimized" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) size_t
fn1 (char *p, char *q)
{
  size_t s = strlen (q);
  strcpy (p, q);
  return s - strlen (p);
}

__attribute__((noinline, noclone)) size_t
fn2 (char *p, char *q)
{
  size_t s = strlen (q);
  memcpy (p, q, s + 1);
  return s - strlen (p);
}

__attribute__((noinline, noclone)) size_t
fn3 (char *p)
{
  memcpy (p, "abcd", 5);
  return strlen (p);
}

__attribute__((noinline, noclone)) size_t
fn4 (char *p)
{
  memcpy (p, "efg\0hij", 6);
  return strlen (p);
}

int
main ()
{
  char buf[64];
  char *volatile p = buf;
  char *volatile q = "ABCDEF";
  buf[7] = 'G';
  if (fn1 (p, q) != 0 || memcmp (buf, "ABCDEF\0G", 8))
    abort ();
  q = "HIJ";
  if (fn2 (p + 1, q) != 0 || memcmp (buf, "AHIJ\0F\0G", 8))
    abort ();
  buf[6] = 'K';
  if (fn3 (p + 1) != 4 || memcmp (buf, "Aabcd\0KG", 8))
    abort ();
  if (fn4 (p) != 3 || memcmp (buf, "efg\0hiKG", 8))
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 4 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
/* { dg-final { scan-tree-dump-times "return 0" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 4" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
