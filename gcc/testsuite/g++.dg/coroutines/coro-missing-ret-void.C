//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

MissingRetVoid bar () {
  co_return; // { dg-error "no member named .return_void. in" }
}

int main (int ac, char *av[]) {
  MissingRetVoid x = bar ();
  return 0;
}
