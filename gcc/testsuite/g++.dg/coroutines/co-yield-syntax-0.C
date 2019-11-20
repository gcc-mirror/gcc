//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

void foo () {
  co_yield;  // { dg-error "expected primary-expression before" }
}
