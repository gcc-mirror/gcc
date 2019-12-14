//  { dg-do run }

// lambda with parm and local state

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int main ()
{
  int a_copy = 20;
  int a_ref = 10;

  auto f = [&, a_copy]() -> coro1
  {
    co_return a_ref + a_copy;
  };

  {
    coro1 A = f ();
    A.handle.resume();
    PRINT ("main: [a_copy = 20, a_ref = 10]");
  
    int y = A.handle.promise().get_value();
    if (y != 30)
      {
	PRINTF ("main: A co-ret = %d, should be 30\n", y);
	abort ();
      }
  }

  a_copy = 5;
  a_ref = 7;

  coro1 B = f ();
  B.handle.resume();
  PRINT ("main: [a_copy = 5, a_ref = 7]");

  int y = B.handle.promise().get_value();
  if (y != 27)
    {
      PRINTF ("main: B co-ret = %d, should be 27\n", y);
      abort ();
    }
  
  return 0;
}
