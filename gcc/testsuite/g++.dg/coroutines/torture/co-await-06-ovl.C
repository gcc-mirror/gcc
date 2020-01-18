//  { dg-do run }

// Basic check of the co_await operator overload.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* A very simple overload.  */
struct empty 
{
  auto operator co_await() const & noexcept { 
    return coro1::suspend_always_intprt{};
  }
};

int gX = 1;
empty e{};

coro1
f ()
{
  int a = co_await(e); /* operator ovl. */
  co_return gX + 5 + a;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  PRINT ("main: got coro1 - checking gX");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      abort ();
    }
  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done'");
      abort ();
    }

  PRINT ("main: resuming [1] initial suspend");
  f_coro.handle.resume();

  PRINT ("main: resuming [2] co_await");
  f_coro.handle.resume();

  /* we should now have returned with the co_return (11) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }

  int y = f_coro.handle.promise().get_value();
  if (y != 11)
    {
      PRINTF ("main: y is wrong : %d, should be 11\n", y);
      abort ();
    }
  puts ("main: done");
  return 0;
}
