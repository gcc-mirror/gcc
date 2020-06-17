//  { dg-do run }

/* check the code-gen for the failed alloc return.
   In this case, we use an operator new that always fails.
   So the g-r-o-o-a-f should fire.  */

#define PROVIDE_GROOAF
#define USE_FAILING_OP_NEW
#include "../coro1-allocators.h"

int used_grooaf = 0;
int used_failing_new = 0;

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  /* nest a scope so that we can check the counts.  */
  {
    PRINT ("main: create coro1");
    struct coro1 x = f ();
    /* we don't expect to be able to do anything.  */
    if (used_failing_new != 1)
      {
	PRINT ("main: we should have used the failing op new");
        abort ();
      }
    if (used_grooaf != 1)
      {
	PRINT ("main: we should have used the GROOAF");
        abort ();
      }
  }
  PRINT ("main: returning");
  return 0;
}
