//  { dg-additional-options "-fsyntax-only -fexceptions -w" }

// Diagose bad coroutine function type.

#include "coro.h"

int
bad_coroutine (void) // { dg-error {coroutine return type 'int' is not a class} }
{
  co_yield 5;
  co_return;
}
