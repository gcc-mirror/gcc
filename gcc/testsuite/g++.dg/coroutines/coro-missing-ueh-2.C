//  { dg-additional-options "-fsyntax-only -fno-exceptions " }

// The missing method is warned for when exceptions are off and pedantic
// is on (default in the testsuite).

#include "coro.h"
#include "coro-missing-ueh.h"

MissingUEH
bar () // { dg-warning {no member named 'unhandled_exception' in} }
{ 
  co_return;
}

int main (int ac, char *av[]) {
  MissingUEH x = bar ();
  return 0;
}
