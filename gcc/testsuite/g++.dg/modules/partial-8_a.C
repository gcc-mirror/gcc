// PR c++/120013
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi m:a }

module;
#include "partial-8.h"
template <typename T> struct tuple_element<T*>;
template <typename T> constexpr int var<T*> = 456;
module m:a;
template <typename T> void a(T t) { ::get(t); foo(t); }
