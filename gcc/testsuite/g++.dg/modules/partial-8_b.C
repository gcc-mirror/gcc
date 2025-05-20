// PR c++/120013
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi m:b }

module;
#include "partial-8.h"
module m:b;
template <typename T> void b(T t) { ::get(t); foo(t); }
