//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

auto x = co_yield 5; // { dg-error {'co_yield' cannot be used outside a function} }
