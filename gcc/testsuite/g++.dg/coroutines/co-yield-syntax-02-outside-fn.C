//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

int a[] = { co_yield 21 }; // { dg-error {'co_yield' cannot be used outside a function} }

