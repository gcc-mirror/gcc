// { dg-do run }
// { dg-require-effective-target dfp }
/* { dg-require-effective-target dfprt } */

extern "C" void abort ();

#include <decimal/decimal>

using namespace std::decimal;

int
foo (double fp)
{
  if (fp < 32.0)
    throw (decimal32)32;
  if (fp < 64.0)
    throw (decimal64)64;
  if (fp < 128.0)
    throw (decimal128)128;
  return 0;
}

int bar (double fp)
{
  try
    {
      foo (fp);
      abort ();
    }
  catch (decimal32 df)
    {
      if (df != (decimal32)32)
	abort ();
    }
  catch (decimal64 dd)
    {
      if (dd != (decimal64)64)
	abort ();
    }
  catch (decimal128 dl)
    {
      if (dl != (decimal128)128)
	abort ();
    }
  return 0;
}

int
main ()
{
  bar (10.0);
  bar (20.0);
  bar (100.0);
  return 0;
}
