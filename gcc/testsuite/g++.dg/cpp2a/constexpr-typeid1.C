// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }
// Test non-polymorphic type.

#include <typeinfo>

struct B {
  const std::type_info &ti = typeid (*this);
};

struct D : B { };

constexpr B b;
constexpr D d;

static_assert (&b.ti == &typeid (B));
static_assert (&B{}.ti == &typeid (B));
static_assert (&B().ti == &typeid (B));
static_assert (&typeid ((B())) == &typeid (B));
static_assert (&typeid ((B{})) == &typeid (B));

static_assert (&d.ti == &typeid (B));
static_assert (&D{}.ti == &typeid (B));
static_assert (&D().ti == &typeid (B));
static_assert (&typeid ((D())) == &typeid (D));
static_assert (&typeid ((D{})) == &typeid (D));

extern D ed;
static_assert (&typeid (ed) == &typeid (D));

constexpr const B &r = d;
static_assert (&typeid (r) == &typeid (B));

constexpr bool
test ()
{
  // If expression is not a glvalue expression of polymorphic type,
  // typeid does not evaluate the expression
  bool ok = true;
  (void) typeid (ok = false, D());
  (void) typeid (ok = false, B());
  (void) typeid (ok = false, b);
  (void) typeid (ok = false, d);
  return ok;
}

static_assert (test ());
