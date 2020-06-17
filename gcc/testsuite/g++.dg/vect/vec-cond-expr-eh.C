/* { dg-do compile } */
/* { dg-additional-options "-fnon-call-exceptions" } */

typedef double v2df __attribute__((vector_size(16)));

v2df foo (v2df a, v2df b, v2df c, v2df d)
{
  try
  {
    v2df res = a < b ? c : d;
    return res;
    }
    catch (...)
    {
    return (v2df){};
    }
}
