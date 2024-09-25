//  { dg-additional-options "-fsyntax-only -w" }

// check error for a bad overload of operator delete.

#define BOGUS_OPDEL_CASE1
#include "coro1-allocators.h"

struct coro1
f ()  /* { dg-error {no suitable 'operator delete' for.*promise_type.*} } */
{
  co_return;
}

