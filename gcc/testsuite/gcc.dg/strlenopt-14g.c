/* This test needs runtime that provides stpcpy and mempcpy functions.  */
/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#define USE_GNU
#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
fn1 (char *p, size_t *l1, size_t *l2)
{
  char *a = mempcpy (p, "abcde", 6);
  /* This strlen needs to stay.  */
  size_t la = strlen (a);
  /* This strlen can be optimized into 5.  */
  size_t lp = strlen (p);
  *l1 = la;
  *l2 = lp;
  return a;
}

__attribute__((noinline, noclone)) char *
fn2 (char *p, const char *q, size_t *l1, size_t *l2, size_t *l3)
{
  /* This strlen needs to stay.  */
  size_t lq = strlen (q);
  char *a = mempcpy (p, q, lq + 1);
  /* This strlen needs to stay.  */
  size_t la = strlen (a);
  /* This strlen can be optimized into lq.  */
  size_t lp = strlen (p);
  *l1 = lq;
  *l2 = la;
  *l3 = lp;
  return a;
}

__attribute__((noinline, noclone)) char *
fn3 (char *p, size_t *l1, size_t *l2)
{
  char *a = stpcpy (p, "abcde");
  /* This strlen can be optimized into 0.  */
  size_t la = strlen (a);
  /* This strlen can be optimized into 5.  */
  size_t lp = strlen (p);
  *l1 = la;
  *l2 = lp;
  return a;
}

__attribute__((noinline, noclone)) char *
fn4 (char *p, const char *q, size_t *l1, size_t *l2, size_t *l3)
{
  /* This strlen needs to stay.  */
  size_t lq = strlen (q);
  char *a = stpcpy (p, q);
  /* This strlen can be optimized into 0.  */
  size_t la = strlen (a);
  /* This strlen can be optimized into lq.  */
  size_t lp = strlen (p);
  *l1 = lq;
  *l2 = la;
  *l3 = lp;
  return a;
}

__attribute__((noinline, noclone)) char *
fn5 (char *p, const char *q, size_t *l1, size_t *l2)
{
  char *a = stpcpy (p, q);
  /* This strlen can be optimized into 0.  */
  size_t la = strlen (a);
  /* This strlen can be optimized into a - p.  */
  size_t lp = strlen (p);
  *l1 = la;
  *l2 = lp;
  return a;
}

int
main ()
{
  char buf[64];
  const char *volatile q = "ABCDEFGH";
  size_t l1, l2, l3;
  memset (buf, '\0', sizeof buf);
  memset (buf + 6, 'z', 7);
  if (fn1 (buf, &l1, &l2) != buf + 6 || l1 != 7 || l2 != 5
      || memcmp (buf, "abcde\0zzzzzzz", 14) != 0)
    abort ();
  if (fn2 (buf, q, &l1, &l2, &l3) != buf + 9 || l1 != 8 || l2 != 4 || l3 != 8
      || memcmp (buf, "ABCDEFGH\0zzzz", 14) != 0)
    abort ();
  if (fn3 (buf, &l1, &l2) != buf + 5 || l1 != 0 || l2 != 5
      || memcmp (buf, "abcde\0GH\0zzzz", 14) != 0)
    abort ();
  l3 = 0;
  memset (buf, 'n', 9);
  if (fn4 (buf, q, &l1, &l2, &l3) != buf + 8 || l1 != 8 || l2 != 0 || l3 != 8
      || memcmp (buf, "ABCDEFGH\0zzzz", 14) != 0)
    abort ();
  memset (buf, 'm', 9);
  if (fn5 (buf, q, &l1, &l2) != buf + 8 || l1 != 0 || l2 != 8
      || memcmp (buf, "ABCDEFGH\0zzzz", 14) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 4 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 1 "strlen" } } */
/* { dg-final { scan-tree-dump-times "mempcpy \\(" 2 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 2 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
