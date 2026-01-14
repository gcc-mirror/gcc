// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_identifier.

#include <meta>

using namespace std::meta;

namespace N
{
  consteval bool
  has_identifier (info r) { return std::meta::has_identifier (r); }
  consteval std::vector<info>
  parameters_of (info r) { return std::meta::parameters_of (r); }
  consteval info
  variable_of (info r) { return std::meta::variable_of (r); }
}

void qux (int, int b, int c, int d, int);
constexpr auto p0 = N::parameters_of (^^qux)[0];
constexpr auto p1 = N::parameters_of (^^qux)[1];
constexpr auto p2 = N::parameters_of (^^qux)[2];
constexpr auto p3 = N::parameters_of (^^qux)[3];
constexpr auto p4 = N::parameters_of (^^qux)[4];
static_assert (!N::has_identifier (p0));
static_assert (N::has_identifier (p1));
static_assert (N::has_identifier (p2));
static_assert (N::has_identifier (p3));
static_assert (!N::has_identifier (p4));
void qux (int a, int, int c, int e, int);
static_assert (N::has_identifier (p0));
static_assert (N::has_identifier (p1));
static_assert (N::has_identifier (p2));
static_assert (!N::has_identifier (p3));
static_assert (!N::has_identifier (p4));

void
qux (int a, int, int, int e, int)
{
  static_assert (N::has_identifier (p0));
  static_assert (N::has_identifier (p1));
  static_assert (N::has_identifier (p2));
  static_assert (!N::has_identifier (p3));
  static_assert (!N::has_identifier (p4));
  static_assert (N::has_identifier (N::variable_of (p0)));
  static_assert (!N::has_identifier (N::variable_of (p1)));
  static_assert (!N::has_identifier (N::variable_of (p2)));
  static_assert (N::has_identifier (N::variable_of (p3)));
  static_assert (!N::has_identifier (N::variable_of (p4)));
}

void qux (int f, int, int, int, int g);
static_assert (!N::has_identifier (p0));
static_assert (N::has_identifier (p1));
static_assert (N::has_identifier (p2));
static_assert (!N::has_identifier (p3));
static_assert (N::has_identifier (p4));
