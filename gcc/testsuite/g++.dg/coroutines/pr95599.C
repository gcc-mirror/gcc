//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// The simplest co_await we can do.

#include "coro.h"

// boiler-plate for tests of codegen
#include "coro1-ret-int-yield-int.h"

int counter = 0;
int a, b, c, d;
int e, f, g;

struct suspend_always_order {
  suspend_always_order (int *v) { 
    PRINT ("susp-always-order-ctor-value"); *v = counter++;
  }
  suspend_always_order () { PRINT ("susp-always-order-ctor"); }
  constexpr bool await_ready() const noexcept { return false; }
  void await_suspend(coro1::handle_type) const noexcept { PRINT ("susp-always-order-susp");}
  void await_resume() const noexcept { PRINT ("susp-always-order-resume");}
  ~suspend_always_order() { PRINT ("susp-always-order-dtor"); }
};

coro1
my_coro ()
{
  (a = counter++, b = counter++, co_await suspend_always_order(&c), d = counter++);
  co_await (e = counter++, suspend_always_order (&f));
  co_return (g = counter++, 10);
}

int main ()
{
  auto f_coro = my_coro ();

  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }
  PRINT ("main: resuming [1] initial suspend");
  f_coro.handle.resume();
  PRINT ("main: resuming [2] co_await");
  f_coro.handle.resume();
  PRINT ("main: resuming [3] co_await");
  f_coro.handle.resume();

  /* we should now have returned with the co_return */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 10)
    {
      PRINTF ("main: y is wrong : %d, should be 10\n", y);
      abort ();
    }
  if (a != 0 || b != 1 || c != 2 || d != 3
      || e != 4 || f != 5 || g != 6 )
    {
      PRINTF ("something happened in the wrong order %d, %d, %d, %d, %d, %d, %d\n", a, b, c, d, e, f, g);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
