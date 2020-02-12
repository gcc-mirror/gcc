//  { dg-do run }

// check codegen for overloaded simple operator new/delete.

#define PROVIDE_NEW_SZT
#define PROVIDE_DEL_VP

#include "../coro1-allocators.h"

int used_ovl_new = 0;
int used_ovl_del = 0;

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  // Nest a scope so that we can inspect the flags after the DTORs run.
  {
  PRINT ("main: create coro1");
  struct coro1 x = f ();
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
  if (used_ovl_new != 1)
    {
      PRINT ("main: failed to call overloaded operator new");
      abort ();
    }
  if (used_ovl_del != 1)
    {
      PRINT ("main: failed to call overloaded operator delete");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
