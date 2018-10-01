//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

auto bar () {
  co_return 5;  // { dg-error "cannot be used in a function with a deduced return type" }
}

int main () {
  bar ();
  return 0;
}
