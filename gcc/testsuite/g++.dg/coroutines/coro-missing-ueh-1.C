//  { dg-additional-options "-fsyntax-only -fexceptions -w" }
#include "coro.h"
#include "coro-missing-ueh.h"

MissingUEH
bar () // { dg-error {no member named 'unhandled_exception' in} }
{ 
  co_return;
}

int main (int ac, char *av[]) {
  MissingUEH x = bar ();
  return 0;
}
