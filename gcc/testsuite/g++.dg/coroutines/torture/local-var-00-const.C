//  { dg-do run }

// Simplest local decl.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f () noexcept
{
  const int answer = 42;
  PRINTF ("coro1: about to return %d\n", answer);
  co_return answer;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
