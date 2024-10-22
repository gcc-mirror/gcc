//  { dg-additional-options "-fsyntax-only -w" }

// check error for missing new (size, nothrow).

#define PROVIDE_NEW_SZT
#define PROVIDE_DEL_VP
#define PROVIDE_GROOAF

#include "coro1-allocators.h"

struct coro1
f () /* { dg-error {'coro1::promise_type::get_return_object_on_allocation_failure\(\)' is provided by 'std::__n4861::__coroutine_traits_impl<coro1, void>::promise_type' \{aka 'coro1::promise_type'\} but 'operator new' is not marked 'throw\(\)' or 'noexcept'} } */
{
  co_return;
}
