// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_template.

#include <meta>
#include <vector>

static_assert (std::meta::is_template (^^std::vector));
static_assert (!std::meta::is_template (^^std::vector<int>));
