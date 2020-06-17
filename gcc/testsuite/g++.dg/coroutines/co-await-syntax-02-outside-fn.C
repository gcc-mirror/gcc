//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

auto f (int x = co_await coro::suspend_always{}); // { dg-error {'co_await' cannot be used outside a function} }
