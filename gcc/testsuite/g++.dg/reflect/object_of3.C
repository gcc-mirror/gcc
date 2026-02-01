// PR c++/123695
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::object_of with invalid arguments.

#include <meta>
using namespace std::meta;

struct A {};
struct B : A {};

using U = int;

enum E { EE };
auto fn ();
[[=1]] void foo ();

consteval bool
object_of_ok (info r)
{
  try { object_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!object_of_ok (^^EE));
static_assert (!object_of_ok (^^::));
static_assert (!object_of_ok (^^int));
static_assert (!object_of_ok (^^U));
static_assert (!object_of_ok (^^fn));
static_assert (!object_of_ok (annotations_of (^^foo)[0]));
static_assert (!object_of_ok (reflect_constant (42)));
static_assert (!object_of_ok (bases_of (^^B, access_context::unchecked ())[0]));
static_assert (!object_of_ok (data_member_spec (^^int, { .name = "dms" })));
