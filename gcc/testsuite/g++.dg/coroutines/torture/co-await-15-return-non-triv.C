//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

/* Check that we handle await_resume for a non-trivial type.  */

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

coro1
f ()
{
  struct test {
    int a;
    ~test () {}
  };
  test input{5};
  test res = co_await coro1::suspend_always_tmpl_awaiter<test>(input);
  co_return res.a + 10;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();

  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }
  PRINT ("main: resuming [1] initial suspend");
  f_coro.handle.resume();
  PRINT ("main: resuming [2] co_await suspend_always_tmpl_awaiter");
  f_coro.handle.resume();

  /* we should now have returned with the co_return (15) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 15)
    {
      PRINTF ("main: y is wrong : %d, should be 15\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
