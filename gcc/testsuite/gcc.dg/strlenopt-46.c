/* PR tree-optimization/86204 - wrong strlen result after prior strnlen
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define NOIPA   __attribute__ ((noipa))

char a[] = "12345";

NOIPA void f0 (void)
{
  unsigned n0 = strnlen (a, 0);
  unsigned n1 = strlen (a);

  if (n0 != 0 || n1 != 5)
    abort ();
}

NOIPA void f1 (void)
{
  unsigned n0 = strnlen (a, 1);
  unsigned n1 = strlen (a);

  if (n0 != 1 || n1 != 5)
    abort ();
}

NOIPA void f2 (void)
{
  unsigned n0 = strnlen (a, 2);
  unsigned n1 = strlen (a);

  if (n0 != 2 || n1 != 5)
    abort ();
}

NOIPA void f3 (void)
{
  unsigned n0 = strnlen (a, 3);
  unsigned n1 = strlen (a);

  if (n0 != 3 || n1 != 5)
    abort ();
}

NOIPA void f4 (void)
{
  unsigned n0 = strnlen (a, 4);
  unsigned n1 = strlen (a);

  if (n0 != 4 || n1 != 5)
    abort ();
}

NOIPA void f5 (void)
{
  unsigned n0 = strnlen (a, 5);
  unsigned n1 = strlen (a);

  if (n0 != 5 || n1 != 5)
    abort ();
}

NOIPA void f6 (void)
{
  unsigned n0 = strnlen (a, 6);
  unsigned n1 = strlen (a);

  if (n0 != 5 || n1 != 5)
    abort ();
}

NOIPA void fx (unsigned n)
{
  unsigned n0 = strnlen (a, n);
  unsigned n1 = strlen (a);

  unsigned min = n < 5 ? n : 5;
  if (n0 != min || n1 != 5)
    abort ();
}

NOIPA void g2 (void)
{
  strcpy (a, "123");
  unsigned n0 = strnlen (a, 2);
  unsigned n1 = strlen (a);

  if (n0 != 2 || n1 != 3)
    abort ();
}

NOIPA void g7 (void)
{
  strcpy (a, "123");
  unsigned n0 = strnlen (a, 7);
  unsigned n1 = strlen (a);

  if (n0 != 3 || n1 != 3)
    abort ();
}

NOIPA void gx (unsigned n)
{
  strcpy (a, "123");
  unsigned n0 = strnlen (a, n);
  unsigned n1 = strlen (a);

  unsigned min = n < 3 ? n : 3;
  if (n0 != min || n1 != 3)
    abort ();
}

int main (void)
{
  f0 ();
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  f6 ();
  fx (2);
  fx (7);

  g2 ();
  g7 ();
  gx (2);
  gx (7);
}
