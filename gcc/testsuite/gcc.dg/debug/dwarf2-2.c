/* { dg-do compile } */

/* Copyright (C) 2000  Free Software Foundation  */
/* Contributed by Alexandre Oliva <aoliva@redhat.com> */

inline double fx (double x)
{
  return 3 * x;
}

main ()
{
  double a = 0, fx (double), foo ();
  fx (a);
  if (a != 3)
    foo ();
}
