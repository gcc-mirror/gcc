// PR c++/88482
// { dg-do compile }

#include <cxxabi.h>

struct S { S (); S (const S &); ~S (); };

int
foo (int x)
{
  if (x > 27)
    throw 19;
  try
    {
      if (x > 15)
	throw S ();
    }
  catch (S s)
    {
      throw;
    }
  return x + 3;
}
