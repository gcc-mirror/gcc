//  { dg-do run }

// lambda with mutable closure object.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* Creates a coro lambda with a mutable closure and
   suspend-always initial suspend.  */

auto make_co_lambda ()
{
  return [i = 1] () mutable -> coro1 { co_return i++; };
}

/* We make this behave sequentially for the purposes of testing.  */
int main()
{
  auto co_l = make_co_lambda ();
  auto v1 = co_l ();
  auto v2 = co_l ();
  auto v3 = co_l ();

  v3.handle.resume();
  v2.handle.resume();
  v1.handle.resume();

  int res1 = v1.handle.promise().get_value ();
  int res2 = v2.handle.promise().get_value ();
  int res3 = v3.handle.promise().get_value ();
  PRINTF ("main: co-lambda %d, %d, %d\n",res1, res2, res3);
  if ( res1 != 3 || res2 != 2 || res3 != 1)
    {
      PRINT ("main: bad return value.");
      abort ();
    }

  if (!v1.handle.done() || !v2.handle.done() || !v3.handle.done())
    {
      PRINT ("main: apparently something was not done...");
      abort ();
    }

  PRINT ("main: done.");
}

