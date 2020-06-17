//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

int main (int ac, char *av[]) {
  co_yield 0; // { dg-error "cannot be used in the .main. function" }
}
