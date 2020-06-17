//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

auto f (co_return); // { dg-error {expected primary-expression before 'co_return'} }
