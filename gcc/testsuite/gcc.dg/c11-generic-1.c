/* Test C11 _Generic.  Valid uses.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Noreturn extern void exit (int);
_Noreturn extern void abort (void);

void
check (int n)
{
  if (n)
    abort ();
}

int
main (void)
{
  int n = 0;

  check (_Generic (n++, int: 0));
  /* _Generic should not evaluate its argument.  */
  check (n);

  check (_Generic (n, double: n++, default: 0));
  check (n);

  /* Qualifiers are removed for the purpose of type matching.  */
  const int cn = 0;
  check (_Generic (cn, int: 0, default: n++));
  check (n);
  check (_Generic ((const int) n, int: 0, default: n++));
  check (n);

  /* Arrays decay to pointers.  */
  int a[1];
  const int ca[1];
  check (_Generic (a, int *: 0, const int *: n++));
  check (n);
  check (_Generic (ca, const int *: 0, int *: n++));
  check (n);

  /* Functions decay to pointers.  */
  extern void f (void);
  check (_Generic (f, void (*) (void): 0, default: n++));
  check (n);

  /* _Noreturn is not part of the function type.  */
  check (_Generic (&abort, void (*) (void): 0, default: n++));
  check (n);

  /* Integer promotions do not occur.  */
  short s;
  check (_Generic (s, short: 0, int: n++));
  check (n);

  exit (0);
}
