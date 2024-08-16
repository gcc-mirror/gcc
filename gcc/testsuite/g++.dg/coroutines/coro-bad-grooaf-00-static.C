/* g-r-o-o-a-f should be static.  */

#define BAD_GROOAF_STATIC
#define PROVIDE_GROOAF
#include "coro1-allocators.h"

int used_grooaf = 0;

struct coro1
f () noexcept // { dg-error {cannot call member function 'coro1 coro1::promise_type::get_return_object_on_allocation_failure\(\)' without object} }
// { dg-error {'get_return_object_on_allocation_failure' is provided by.*} "" { target *-*-* } .-1 }
{
  PRINT ("coro1: about to return");
  co_return;
}
