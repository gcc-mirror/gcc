// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_object.

#include <meta>

using namespace std::meta;

const char *s = "foo";
constexpr auto r1 = reflect_object (s);
extern int x;
constexpr auto r2 = reflect_object (x);
int ai[10];
constexpr auto r3 = reflect_object (ai);

constexpr auto e1 = reflect_object (42); // { dg-error "cannot bind" }

struct J1 {
  J1 *self = this;
};
constexpr auto e2 = reflect_object (J1{}); // { dg-error "cannot bind" }

struct J2 {
  J2 *self = this;
  constexpr J2() {}
  constexpr J2(const J2&) {}
};
constexpr auto e3 = reflect_object (J2{}); // { dg-error "cannot bind" }

struct X { int n; };
struct Y { const int &r; };
constexpr auto e4 = reflect_object (Y{X{1}.n}); // { dg-error "cannot bind" }

void
f1 ()
{
  constexpr int& ref = x;
  constexpr auto rref = reflect_object (ref);
}

consteval bool
can_reflect_object_on_str ()
{
  try { reflect_object ("foo"); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!can_reflect_object_on_str ());

extern int& vref;

consteval bool
can_reflect_extern_reference ()
{
  try { reflect_object (vref); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert(!can_reflect_extern_reference());
