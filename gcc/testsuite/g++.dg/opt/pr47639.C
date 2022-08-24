// { dg-do compile }
// { dg-options "-fnon-call-exceptions -Wno-psabi" }

typedef int __attribute__ ((vector_size (8))) vec;

vec foo (vec v1, vec v2)
{
  try
    {
      return v1 / v2;
    }
  catch (...)
    {
      throw;
    }
}

