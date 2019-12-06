//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

int main (int ac, char *av[]) {
  co_await std::experimental::suspend_always{}; // { dg-error "cannot be used in the .main. function" }
}
