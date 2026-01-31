// PR c++/123825
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::identifier_of.

#include <meta>

void foo (int, int x, int y, int z);

int
bar (int, int x, int y, int z, int)
{
  return x + y + z;
}

constexpr auto foo1 = parameters_of (^^foo)[0];
constexpr auto foo2 = parameters_of (^^foo)[1];
constexpr auto foo3 = parameters_of (^^foo)[2];
constexpr auto foo4 = parameters_of (^^foo)[3];
constexpr auto bar1 = parameters_of (^^bar)[0];
constexpr auto bar2 = parameters_of (^^bar)[1];
constexpr auto bar3 = parameters_of (^^bar)[2];
constexpr auto bar4 = parameters_of (^^bar)[3];
constexpr auto bar5 = parameters_of (^^bar)[4];
static_assert (!has_identifier (foo1));
static_assert (identifier_of (foo2) == std::string_view ("x"));
static_assert (identifier_of (foo3) == std::string_view ("y"));
static_assert (identifier_of (foo4) == std::string_view ("z"));
static_assert (!has_identifier (bar1));
static_assert (identifier_of (bar2) == std::string_view ("x"));
static_assert (identifier_of (bar3) == std::string_view ("y"));
static_assert (identifier_of (bar4) == std::string_view ("z"));
static_assert (!has_identifier (bar5));

void
baz ()
{
  void foo (int w, int, int v, int z);
  int bar (int, int, int v, int z, int u);
  void qux (int, int x, int y, int z);
  constexpr auto qux1 = parameters_of (^^qux)[0];
  constexpr auto qux2 = parameters_of (^^qux)[1];
  constexpr auto qux3 = parameters_of (^^qux)[2];
  constexpr auto qux4 = parameters_of (^^qux)[3];
  static_assert (!has_identifier (qux1));
  static_assert (identifier_of (qux2) == std::string_view ("x"));
  static_assert (identifier_of (qux3) == std::string_view ("y"));
  static_assert (identifier_of (qux4) == std::string_view ("z"));
}

static_assert (identifier_of (foo1) == std::string_view ("w"));
static_assert (identifier_of (foo2) == std::string_view ("x"));
static_assert (!has_identifier (foo3));
static_assert (identifier_of (foo4) == std::string_view ("z"));
static_assert (!has_identifier (bar1));
static_assert (identifier_of (bar2) == std::string_view ("x"));
static_assert (!has_identifier (bar3));
static_assert (identifier_of (bar4) == std::string_view ("z"));
static_assert (identifier_of (bar5) == std::string_view ("u"));

void
fred ()
{
  void qux (int w, int, int v, int z);
  constexpr auto qux1 = parameters_of (^^qux)[0];
  constexpr auto qux2 = parameters_of (^^qux)[1];
  constexpr auto qux3 = parameters_of (^^qux)[2];
  constexpr auto qux4 = parameters_of (^^qux)[3];
  static_assert (identifier_of (qux1) == std::string_view ("w"));
  static_assert (identifier_of (qux2) == std::string_view ("x"));
  static_assert (!has_identifier (qux3));
  static_assert (identifier_of (qux4) == std::string_view ("z"));
}
