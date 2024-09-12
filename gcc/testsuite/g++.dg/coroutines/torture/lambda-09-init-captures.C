//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// lambda with initialized captures

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int main ()
{
  int a_copy = 20;

  auto f = [&a_ref = a_copy, a_copy = a_copy + 10]() -> coro1
  {
    a_ref += 20;
    co_return a_ref + a_copy;
  };

  {
    coro1 A = f ();
    A.handle.resume();
    if (a_copy != 40)
      {
        PRINT ("main: [a_copy = 40]");
	abort ();
      }
  
    int y = A.handle.promise().get_value();
    if (y != 70)
      {
	PRINTF ("main: A co-ret = %d, should be 70\n", y);
	abort ();
      }
  }

  a_copy = 5;

  coro1 B = f ();
  B.handle.resume();
  if (a_copy != 25)
    {
      PRINT ("main: [a_copy = 25]");
      abort ();
    }

  int y = B.handle.promise().get_value();
  if (y != 55)
    {
      PRINTF ("main: B co-ret = %d, should be 55\n", y);
      abort ();
    }
  
  return 0;
}
