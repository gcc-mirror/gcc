// We cannot compile this yet, much run it - but one day it might be
// feasible, so do the minimum for now.
// { dg-additional-options " -fsyntax-only -Wno-vla" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "coro.h"

// boiler-plate for tests of codegen
#include "coro1-ret-int-yield-int.h"

struct coro1
foo (int arg) noexcept
{
  PRINTF ("foo arg = %d\n", arg);
  char arr[arg]; /* { dg-message "sorry, unimplemented: variable length arrays are not yet supported in coroutines" "" { target *-*-* } } */
  if (arg < 4)
    co_return -6174;
  else
    for (int i = 0; i < arg; ++i) arr[i] = (char) i;
  co_yield (int) arr[2];
  co_return (int) arr[3];
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = foo (10);
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y == -6174 )
    {
      PRINT ("main: saw -6174");
      return 1;
    }
  else if ( y != 2 )
    abort;
  x.handle.resume();
  y = x.handle.promise().get_value();
  if ( y != 3 )
    abort ();
  return 0;
}
