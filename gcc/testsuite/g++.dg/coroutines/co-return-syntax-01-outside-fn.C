//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

co_return; // { dg-error {expected unqualified-id before 'co_return'} }

