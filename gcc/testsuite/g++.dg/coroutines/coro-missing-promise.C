//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

// Diagnose completely missing promise.

struct NoPromiseHere {
  coro::coroutine_handle<> handle;
  NoPromiseHere () : handle (nullptr) {}
  NoPromiseHere (coro::coroutine_handle<> handle) : handle (handle) {}
};

NoPromiseHere
bar ()
{
  co_yield 22; // { dg-error {unable to find the promise type for this coroutine} }
  co_return 0;
}

// check we have not messed up continuation of the compilation.
template <class... Args>
struct void_t_imp {
  using type = void;
};
