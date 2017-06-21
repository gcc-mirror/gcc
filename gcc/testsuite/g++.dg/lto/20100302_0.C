// Test for ABI forward-compatibility aliases with LTO.
// { dg-skip-if ""  { { ! { i?86-*-* x86_64-*-* } } || { *-*-darwin* } } }
// { dg-lto-options {"-flto -fabi-version=2"} }

#include "20100302.h"

void f(mm128 *) { }

template <> mm128 A<mm128>::t = { };
