// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_complete_type.

#include <meta>

using namespace std::meta;

struct S;

namespace N
{
  consteval bool
  is_complete_type (info r) { return std::meta::is_complete_type (r); }
}

static_assert (!is_complete_type (^^S));
static_assert (!N::is_complete_type (^^S));

consteval {
  define_aggregate (^^S, {});
}

static_assert (is_complete_type (^^S));
static_assert (N::is_complete_type (^^S));
