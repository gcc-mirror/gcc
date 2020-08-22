//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

// Check diagnostic return from missing promise initial suspend entry.

#define MISSING_INITIAL_SUSPEND
#include "coro1-ret-int-yield-int.h"

coro1
my_coro () // { dg-error {no member named 'initial_suspend' in} }
{
  co_return 0;
}

// check we have not messed up continuation of the compilation.
template <class... Args>
struct void_t_imp {
  using type = void;
};
