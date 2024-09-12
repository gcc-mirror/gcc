//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// check references are handled as expected.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

coro1 
f (int& a_ref, int a_copy)
{
    co_yield a_ref + a_copy;
    co_return a_ref + a_copy;
}

int main ()
{
  int a_copy = 20;
  int a_ref = 10;

  coro1 A = f (a_ref, a_copy);
  A.handle.resume(); // Initial suspend.
  PRINT ("main: [a_copy = 20, a_ref = 10]");
  
  int y = A.handle.promise().get_value();
  if (y != 30)
    {
      PRINTF ("main: co-yield = %d, should be 30\n", y);
      abort ();
    }

  a_copy = 5;
  a_ref = 7;

  A.handle.resume();
  PRINT ("main: [a_copy = 5, a_ref = 7]");

  y = A.handle.promise().get_value();
  if (y != 27)
    {
      PRINTF ("main: co-ret = %d, should be 27\n", y);
      abort ();
    }
  
  return 0;
}
