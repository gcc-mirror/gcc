/* Check that gcov correctly rounds nearly zero to nonzero and nearly
   100 to not-100.

   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>
*/

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int proxy (int i)
{
  return i;
}

int foo (int i)
{
  if (i > 0) /* branch(1) */
    return proxy (1);
  else if (i < 0) /* branch(100) */
    return proxy (-1);
  else
    return proxy (0);
}

int baz (int i)
{
  if (i == 0) /* branch(99) */
    return proxy (0);
  else if (i > 0) /* branch(0)*/
    return proxy (1);
  else
    return proxy (-1);
}

int main ()
{
  int t = 0;
  int ix;

  for (ix = 0; ix != 1000; ix++)
    t += foo (ix) + baz (ix);
  
  return t == 0;
}

/* { dg-final { run-gcov -b gcov-8.c } } */
