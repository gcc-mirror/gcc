// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_function.

#include <meta>

using namespace std::meta;
using namespace std::literals;

void foo ();
static_assert (reflect_function (::foo) == ^^::foo);
static_assert (is_function (reflect_function (::foo)));
static_assert (!is_object (reflect_function (::foo)));
static_assert (!is_value (reflect_function (::foo)));
static_assert (type_of (reflect_function (::foo)) == ^^void ());
static_assert (identifier_of (reflect_function (::foo)) == "foo"sv);

const auto &ref = ::foo;
static_assert (reflect_function (ref) == ^^::foo);
static_assert (is_function (reflect_function (ref)));
static_assert (!is_object (reflect_function (ref)));
static_assert (!is_value (reflect_function (ref)));
static_assert (type_of (reflect_function (ref)) == ^^void());
static_assert (identifier_of (reflect_function (ref)) == "foo"sv);

constexpr void (*fp)() = ::foo;
static_assert (reflect_function (*fp) == ^^::foo);
static_assert (is_function (reflect_function (*fp)));
static_assert (!is_object (reflect_function (*fp)));
static_assert (!is_value (reflect_function (*fp)));
static_assert (type_of (reflect_function (*fp)) == ^^void());
static_assert (identifier_of (reflect_function (*fp)) == "foo"sv);

template <void(&P)()>
void
fn_fn_ref_param ()
{
  static constexpr auto R = reflect_function (P);
  static_assert (is_function (R));
  static_assert (type_of (R) == ^^void ());
  static_assert (identifier_of (R) == "doit"sv);
}

constexpr int bar (int i) { return i + 42; }
static_assert ([:reflect_function (bar):](0) == 42);

template<int N>
constexpr int
baz (int i)
{
  return i + N;
}
static_assert ([:reflect_function (baz<1>):](1) == 2);

void
doit ()
{
  fn_fn_ref_param<doit>();
  [:reflect_function (::foo):]();
  [:reflect_function (::ref):]();
  [:reflect_function (*::fp):]();
}

extern int (&fref)();

consteval bool
can_reflect_extern_reference ()
{
  try { reflect_function (fref); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert(!can_reflect_extern_reference());
