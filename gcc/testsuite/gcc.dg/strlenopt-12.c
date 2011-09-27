/* { dg-do run } */
/* { dg-options "-O2" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
fn1 (char *p, size_t *l)
{
  char *q = strcat (p, "abcde");
  *l = strlen (p);
  return q;
}

__attribute__((noinline, noclone)) char *
fn2 (char *p, const char *q, size_t *l1, size_t *l2)
{
  size_t l = strlen (q);
  char *r = strcat (p, q);
  *l1 = l;
  *l2 = strlen (p);
  return r;
}

__attribute__((noinline, noclone)) char *
fn3 (char *p, const char *q, size_t *l)
{
  char *r = strcpy (p, q);
  *l = strlen (p);
  return r;
}

__attribute__((noinline, noclone)) char *
fn4 (char *p, const char *q, size_t *l)
{
  char *r = strcat (p, q);
  *l = strlen (p);
  return r;
}

__attribute__((noinline, noclone)) char *
fn5 (char *p, const char *q, size_t *l1, size_t *l2, size_t *l3)
{
  size_t l = strlen (q);
  size_t ll = strlen (p);
  char *r = strcat (p, q);
  *l1 = l;
  *l2 = strlen (p);
  *l3 = ll;
  return r;
}

__attribute__((noinline, noclone)) char *
fn6 (char *p, const char *q, size_t *l1, size_t *l2)
{
  size_t l = strlen (p);
  char *r = strcat (p, q);
  *l1 = strlen (p);
  *l2 = l;
  return r;
}

int
main ()
{
  char buf[64];
  const char *volatile q = "fgh";
  size_t l, l1, l2, l3;
  memset (buf, '\0', sizeof buf);
  memset (buf, 'a', 3);
  if (fn1 (buf, &l) != buf || l != 8 || memcmp (buf, "aaaabcde", 9) != 0)
    abort ();
  if (fn2 (buf, q, &l1, &l2) != buf || l1 != 3 || l2 != 11
      || memcmp (buf, "aaaabcdefgh", 12) != 0)
    abort ();
  if (fn3 (buf, q, &l) != buf || l != 3
      || memcmp (buf, "fgh\0bcdefgh", 12) != 0)
    abort ();
  if (fn4 (buf, q, &l) != buf || l != 6
      || memcmp (buf, "fghfgh\0efgh", 12) != 0)
    abort ();
  l1 = 0;
  l2 = 0;
  if (fn5 (buf, q, &l1, &l2, &l3) != buf || l1 != 3 || l2 != 9 || l3 != 6
      || memcmp (buf, "fghfghfgh\0h", 12) != 0)
    abort ();
  if (fn6 (buf, q, &l1, &l2) != buf || l1 != 12 || l2 != 9
      || memcmp (buf, "fghfghfghfgh", 13) != 0)
    abort ();
  return 0;
}
