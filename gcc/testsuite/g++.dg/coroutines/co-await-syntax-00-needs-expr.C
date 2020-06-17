//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

void bar () {
  co_await;  // { dg-error "expected primary-expression before" }
}
