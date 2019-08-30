// PR tree-optimization/90090
// { dg-do compile }
// { dg-options "-Ofast -fno-associative-math -fsignaling-nans -fno-tree-dce -fnon-call-exceptions" }

double bar (double, double, double, double, double);
double baz ();

double
foo (double a)
{
  try
    {
      return bar (1.0/a, 2.0/a, 4.0/a, 8.0/a, 16.0/a);
    }
  catch (...)
    {
      return baz ();
    }
}
