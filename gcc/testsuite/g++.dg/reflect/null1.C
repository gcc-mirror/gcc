// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test null reflection.

#include <meta>

struct S {};

static_assert(std::meta::info() == std::meta::info());
static_assert(std::meta::info{} == std::meta::info{});
static_assert(std::meta::info() != ^^S);
static_assert(std::meta::info{} != ^^S);

constexpr std::meta::info g;
constexpr std::meta::info g2{};
static_assert(g == std::meta::info());
static_assert(g == g2);
constexpr std::meta::info *g3{};
constexpr std::meta::info *g4{};

consteval void
f ()
{
  std::meta::info m;
  std::meta::info m2{};
  static std::meta::info m3;
  static std::meta::info m4{};
  static constexpr std::meta::info m5;
  static constexpr std::meta::info m6{};
}
