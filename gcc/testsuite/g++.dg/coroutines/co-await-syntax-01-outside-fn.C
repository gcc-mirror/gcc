//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

int x = co_await std::experimental::suspend_always{}; // { dg-error {'co_await' cannot be used outside a function} }
