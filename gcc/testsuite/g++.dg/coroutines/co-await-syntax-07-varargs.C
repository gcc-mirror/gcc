//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

int
bar (int x, ...)
{
  co_await std::experimental::suspend_always{}; // { dg-error "cannot be used in a varargs function" }
}

int main (int ac, char *av[]) {
  bar (5, ac);
  return 0;
}
