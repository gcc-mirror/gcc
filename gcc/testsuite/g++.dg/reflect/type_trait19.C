// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], type properties.

#include <meta>
using namespace std::meta;

using U = std::meta::info;
using A = U[5];
struct S { std::meta::info i; };

static_assert (!has_unique_object_representations (^^std::meta::info));
static_assert (!has_unique_object_representations (^^U));
static_assert (!has_unique_object_representations (^^S));
static_assert (!has_unique_object_representations (^^A));
