// PR target/70245
// { dg-do compile }
// { dg-options "" }

#include "pr70245.h"

D m;
A n, o;
int p, q;

int *
fn1 (char *x, int *y)
{
  *y = 0;
  return &p;
}

void
fn2 ()
{
  __builtin_abort ();
}

void *
fn3 (int *x)
{
  *x = 0;
  return (void *) &m;
}

void *
fn4 ()
{
  a = &o;
  o.a1.d = 9;
  m.d = sizeof (D);
  __builtin_memcpy (o.a2.c, "abcdefghijklmnop", 16);
  return (void *) &n;
}

void
fn5 (A *x, B *y, unsigned char *z, int *w)
{
  if (x != &n || y != &k || z != (unsigned char *) (&m + 1))
    __builtin_abort ();
  q++;
}

int
main ()
{
  d = fn5;
  baz (0);
  if (q != 1)
    __builtin_abort ();
}
