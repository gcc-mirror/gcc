// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fmodules" }
// Test std::meta::has_external_linkage.

module;

#include <meta>

module has_external_linkage:counter;

int counter = 0;

static_assert (!std::meta::has_external_linkage (^^counter));
