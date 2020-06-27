/* g-r-o-o-a-f would be expected, since we have a noexcept op new.  */

#define USE_FAILING_OP_NEW
#include "coro1-allocators.h"

int used_grooaf = 0;

struct coro1
f () noexcept // { dg-warning {'operator new' is marked 'throw\(\)' or 'noexcept' but no usable 'get_return_object_on_allocation_failure' is provided by 'std::__n4861::__coroutine_traits_impl<coro1, void>::promise_type' \{aka 'coro1::promise_type'\}} }
{
  PRINT ("coro1: about to return");
  co_return;
} 

