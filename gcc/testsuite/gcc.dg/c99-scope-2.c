/* Test for new block scopes in C99.  Test for each new scope.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  struct foo { int i0; };
  int a, b, c, d;
  a = sizeof (struct foo);
  if (b = sizeof (struct foo { int i0; int i1; }))
    c = sizeof (struct foo { int i0; int i1; int i2; });
  if (!(a <= b && b <= c))
    abort ();
  if ((b = sizeof (struct foo { int i0; int i1; })), 0)
    c = sizeof (struct foo { int i0; int i1; int i2; });
  else
    d = sizeof (struct foo { int i0; int i1; int i2; int i3; });
  if (!(a <= b && b <= d))
    abort ();
  switch (b = sizeof (struct foo { int i0; int i1; }))
    default:
      c = sizeof (struct foo { int i0; int i1; int i2; });
  if (!(a <= b && b <= c))
    abort ();
  do
    c = sizeof (struct foo { int i0; int i1; int i2; });
  while ((b = sizeof (struct foo { int i0; int i1; })), 0);
  if (!(a <= b && b <= c))
    abort ();
  d = 1;
  while ((b = sizeof (struct foo { int i0; int i1; })), d)
    (c = sizeof (struct foo { int i0; int i1; int i2; })), d--;
  if (!(a <= b && b <= c))
    abort ();
  d = 1;
  for ((b = sizeof (struct foo { int i0; int i1; })); d; d--)
    c = sizeof (struct foo { int i0; int i1; int i2; });
  if (!(a <= b && b <= c))
    abort ();
  d = 1;
  for ((b = sizeof (struct foo { int i0; int i1; })); d; d--)
    c = sizeof (struct foo);
  if (!(a <= b && b == c))
    abort ();
  d = 1;
  for (; (b = sizeof (struct foo { int i0; int i1; })), d; d--)
    c = sizeof (struct foo { int i0; int i1; int i2; });
  if (!(a <= b && b <= c))
    abort ();
  d = 1;
  for (; (b = sizeof (struct foo { int i0; int i1; })), d; d--)
    c = sizeof (struct foo);
  if (!(a <= b && b == c))
    abort ();
  d = 1;
  for (; d; (b = sizeof (struct foo { int i0; int i1; })), d--)
    c = sizeof (struct foo { int i0; int i1; int i2; });
  if (!(a <= b && b <= c))
    abort ();
  d = 1;
  for (; d; (b = sizeof (struct foo { int i0; int i1; })), d--)
    c = sizeof (struct foo);
  if (!(a <= b && b == c))
    abort ();
  exit (0);
}
