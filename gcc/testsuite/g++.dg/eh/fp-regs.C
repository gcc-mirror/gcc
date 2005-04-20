// PR 20670: f29 corrupted when unwind stack.  This tries to test that FP
// registers are properly saved and restored by defining 20 different FP
// local variables.
// { dg-do run }
// { dg-options "-O" }
#include <stdlib.h>
 
double zero = 0.0;
double another_zero = 0.0;
 
int
sub (void)
{
  throw (0);
}
 
int
main (void)
{
  double a, b, c, d, e, f, g, h, i, j;
  double a1, b1, c1, d1, e1, f1, g1, h1, i1, j1;
 
  a = zero;
  b = a + 1;
  c = b + 1;
  d = c + 1;
  e = d + 1;
  f = e + 1;
  g = f + 1;
  h = g + 1;
  i = h + 1;
  j = i + 1;
   
  a1 = another_zero;
  b1 = a1 + 1;
  c1 = b1 + 1;
  d1 = c1 + 1;
  e1 = d1 + 1;
  f1 = e1 + 1;
  g1 = f1 + 1;
  h1 = g1 + 1;
  i1 = h1 + 1;
  j1 = i1 + 1;
   
  try
    {
      sub ();
    }
  catch (...)
    {
      if (a != 0.0)
        abort ();
      if (b != 1.0)
        abort ();
      if (c != 2.0)
        abort ();
      if (d != 3.0)
        abort ();
      if (e != 4.0)
        abort ();
      if (f != 5.0)
        abort ();
      if (g != 6.0)
        abort ();
      if (h != 7.0)
        abort ();
      if (i != 8.0)
        abort ();
      if (j != 9.0)
        abort ();
 
      if (a1 != 0.0)
        abort ();
      if (b1 != 1.0)
        abort ();
      if (c1 != 2.0)
        abort ();
      if (d1 != 3.0)
        abort ();
      if (e1 != 4.0)
        abort ();
      if (f1 != 5.0)
        abort ();
      if (g1 != 6.0)
        abort ();
      if (h1 != 7.0)
        abort ();
      if (i1 != 8.0)
        abort ();
      if (j1 != 9.0)
        abort ();
    }
  return 0;
}
