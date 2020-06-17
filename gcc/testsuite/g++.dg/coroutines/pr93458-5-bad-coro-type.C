//  { dg-additional-options "-fsyntax-only -fexceptions -w" }

// Diagose bad coroutine function type.

#include "coro.h"

int
bad_coroutine (void) 
{
  co_yield 5; // { dg-error {unable to find the promise type for this coroutine} }
  co_return;
}
