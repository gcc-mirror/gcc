//  { dg-do run }

// Class with parm capture

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

class foo
{
  public:
  coro1 meth (int x)
    {
      if (x > 30)
	{
	  PRINT ("coro1: about to return k");
	  co_return 6174;
	}
      else if (x > 20)
	{
	  PRINT ("coro1: about to return the answer");
	  co_return 42;
	}
      else
	{
	  PRINT ("coro1: about to return 0");
	  co_return 0;
	}
    }
};

int main ()
{
  foo inst;

  PRINT ("main: create coro1");
  coro1 x = inst.meth (25);
  if (x.handle.done())
    abort();

  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    {
      PRINTF ("main: wrong result (%d)", y);
      abort ();
    }
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
