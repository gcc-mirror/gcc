// PR c++/36191
// { dg-do compile }
// { dg-options "-fnon-call-exceptions" }

__complex__ double
foo (__complex__ double x, double y)
{
  try
    {
      return x / y;
    }
  catch (char *s)
    {
      return x;
    }
}
