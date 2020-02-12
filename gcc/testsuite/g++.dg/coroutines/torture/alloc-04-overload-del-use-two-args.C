//  { dg-do run }

/* check that we use the deallocation function with two args when both
   are available.  */

#define PROVIDE_NEW_SZT
#define PROVIDE_DEL_VP
#define PROVIDE_DEL_VP_SZT

#include "../coro1-allocators.h"

int used_ovl_new = 0;
int used_ovl_del = 0;
int used_ovl_del_2arg = 0;

struct coro1
f (int v) noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  // Nest a scope so that we can inspect the flags after the DTORs run.
  {
  PRINT ("main: create coro1");
  struct coro1 x = f (5);

  if (used_ovl_new != 1)
    {
      PRINT ("main: apparently used the wrong op new...");
      abort ();
    }

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
  }
  if (used_ovl_del != 0)
    {
      PRINT ("main: wrong call to overloaded operator delete 1 arg");
      abort ();
    }

  if (used_ovl_del_2arg != 1)
    {
      PRINT ("main: failed to call overloaded operator delete 2 args");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
