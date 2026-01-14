// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

class A {
  int a = 0;
  consteval int foo () const { return 5; }
public:
  int b = 0;
  consteval A () = default;
  consteval A (int x, int y) : a (x), b (y) {}
};

constexpr auto ctx = std::meta::access_context::unchecked ();
static_assert (identifier_of (members_of (^^A, ctx)[0]) == "a");
static_assert (identifier_of (members_of (^^A, ctx)[1]) == "foo");
static_assert (identifier_of (members_of(^^A, ctx)[2]) == "b");

constexpr A c, d = { 42, 6 };
// TODO, these don't work yet due to access failure.
static_assert (c.[: members_of (^^A, ctx)[0] :] == 0);
//static_assert (c.[: members_of (^^A, ctx)[1] :] () == 5);
static_assert (c.[: members_of (^^A, ctx)[2] :] == 0);
static_assert (d.[: members_of (^^A, ctx)[0] :] == 42);
//static_assert (d.[: members_of (^^A, ctx)[1] :] () == 5);
static_assert (d.[: members_of (^^A, ctx)[2] :] == 6);
