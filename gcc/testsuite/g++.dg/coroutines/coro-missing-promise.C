//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

// Diagnose completely missing promise.

// { dg-error {no type named 'promise_type' in 'struct NoPromiseHere'} "" { target *-*-* } 0 }

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
