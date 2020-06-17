//  { dg-additional-options "-fsyntax-only -w" }

// Check that we decline return type deduction for lambda coroutines.

#include "coro.h"

// boiler-plate for tests of codegen
#include "coro1-ret-int-yield-int.h"

int main ()
{
  /* Attempt to deduce the return type for a lambda coroutine.  */
  auto f = []()
  {
    co_yield 42; // { dg-error "cannot be used in a function with a deduced return type" }
  };

  return 0;
}
