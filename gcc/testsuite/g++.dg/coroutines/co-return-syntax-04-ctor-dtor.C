//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

struct Foo {
  Foo ()  { co_return; } // { dg-error "cannot be used in a constructor" }
  ~Foo () { co_return 5; } // { dg-error "cannot be used in a destructor" }
};
