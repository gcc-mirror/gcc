//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

struct Foo {
  Foo ()  { co_await std::experimental::suspend_always{}; } // { dg-error "cannot be used in a constructor" }
  ~Foo () { co_await std::experimental::suspend_always{}; } // { dg-error "cannot be used in a destructor" }
};
