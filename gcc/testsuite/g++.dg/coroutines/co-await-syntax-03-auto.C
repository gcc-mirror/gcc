//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

extern struct awaitable *aw ();

auto bar () {
  int x = 1 + co_await *aw();  // { dg-error "cannot be used in a function with a deduced return type" }
  
  return x;
}

int main () {
  bar ();
  return 0;
}
