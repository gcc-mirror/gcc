// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }
// Test that reflecting names from modules works, and we deduplicate correctly.

#include <meta>

namespace ns {
  inline void b() {}  // dedup with reflect-1_a.H and M GMF
  inline void d() {}  // dedup with M purview
  inline void f() {}
}

import "reflect-2_a.H";
import M;

constexpr auto ctx = std::meta::access_context::unchecked();
consteval int count_name(std::meta::info i, const char* name) {
  int count = 0;
  for (std::meta::info x : members_of(i, ctx))
    if (has_identifier(x) && identifier_of(x) == name)
      ++count;
  return count;
}

static_assert(count_name(^^ns, "a") == 1);
static_assert(count_name(^^ns, "b") == 1);
static_assert(count_name(^^ns, "c") == 0);  // discarded from M's GMF
static_assert(count_name(^^ns, "d") == 1);
static_assert(count_name(^^ns, "e") == 0);  // e@M is not visible from here
static_assert(count_name(^^ns, "f") == 2);  // f and f@M are separate entities

// handle imported stat hack
static_assert(count_name(^^ns, "S") == 2);
static_assert(count_name(^^ns, "T") == 1);
static_assert(count_name(^^ns, "U") == 1);

static_assert(members_of(^^ns, ctx).size() == 9);
