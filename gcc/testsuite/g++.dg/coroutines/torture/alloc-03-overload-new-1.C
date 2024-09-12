//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

/* check codegen for overloaded simple operator new/delete.
   here check that we prefer the overload that accounts the function
   args.  */

#define PROVIDE_NEW_SZT
#define PROVIDE_NEW_SZT_INT
#define PROVIDE_DEL_VP

#include "../coro1-allocators.h"

int used_ovl_new = 0;
int used_ovl_del = 0;

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

  if (used_ovl_new != 5)
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

  if (used_ovl_del != 1)
    {
      PRINT ("main: failed to call overloaded operator delete");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
