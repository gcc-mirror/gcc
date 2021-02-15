//  { dg-do run }

// Test that we can manage a constructed param copy.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

// Require a ctor.
struct nontriv {
  int a, b, c;
  nontriv (int _a, int _b, int _c) : a(_a), b(_b), c(_c) {}
  virtual int getA () { return a; }
  ~nontriv() { a = -1; }
};

struct coro1
f (nontriv t) noexcept
{
  if (t.a > 30)
    {
      PRINTF ("coro1: about to return %d\n", t.b);
      co_return t.b;
    }
  else if (t.a > 20)
    {
      PRINTF ("coro1: about to co-return %d\n", t.c);
      co_return t.c;
    }
  else
    {
      PRINT ("coro1: about to return 0");
      co_return 0;
    }
}

int main ()
{
  PRINT ("main: create coro1");
  nontriv test (25, 6174, 42);
  struct coro1 x = f (test);
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
