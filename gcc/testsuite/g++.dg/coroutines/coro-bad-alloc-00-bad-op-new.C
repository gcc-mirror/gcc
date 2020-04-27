//  { dg-additional-options "-fsyntax-only -w" }

// check error for a bad overload of operator new.

#define BOGUS_OPNEW_CASE1
#include "coro1-allocators.h"

struct coro1
f ()  /* { dg-error {'operator new' is provided by 'std::__n4861::__coroutine_traits_impl<coro1, void>::promise_type' \{aka 'coro1::promise_type'\} but is not usable with the function signature 'coro1 f\(\)'} } */
{
  co_return;
}
