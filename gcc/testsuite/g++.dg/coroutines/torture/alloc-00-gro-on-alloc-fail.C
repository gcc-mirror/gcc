//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

/* check the code-gen for the failed alloc return.
   Here we use an allocator that doesn't fail so that the code
   is generated, but the regular path is run.  */

#define PROVIDE_GROOAF
#include "../coro1-allocators.h"

int used_grooaf = 0;

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  if (used_grooaf)
    abort ();

  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
