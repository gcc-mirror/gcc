//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

struct Foo {
  Foo ()  { co_yield 4; } // { dg-error "cannot be used in a constructor" }
  ~Foo () { co_yield 4; } // { dg-error "cannot be used in a destructor" }
};
