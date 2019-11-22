// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }
// Test polymorphic type.

#include <typeinfo>

struct B {
  virtual void foo ();
  const std::type_info &ti_base = typeid (*this);
};

struct D : B {
  const std::type_info &ti = typeid (*this);
};

constexpr B b;
constexpr D d;

static_assert (&b.ti_base == &typeid (B));
static_assert (&B{}.ti_base == &typeid (B));
static_assert (&B().ti_base == &typeid (B));
static_assert (&typeid ((B())) == &typeid (B));
static_assert (&typeid ((B{})) == &typeid (B));

static_assert (&d.ti == &typeid (D));
static_assert (&D{}.ti == &typeid (D));
static_assert (&D().ti == &typeid (D));
static_assert (&typeid ((D())) == &typeid (D));
static_assert (&typeid ((D{})) == &typeid (D));

extern D ed;
// ??? Should this succeed?
static_assert (&typeid (ed) == &typeid (D));

constexpr const B &r = d;
static_assert (&typeid (r) == &typeid (D));

constexpr bool
test ()
{
  // If expression is a glvalue expression that identifies an object
  // of a polymorphic type, the typeid expression evaluates the expression.
  bool ok = true;
  // Not a glvalue.
  (void) typeid (ok = false, D());
  (void) typeid (ok = false, B());
  if (!ok)
    return false;

  // Polymorphic glvalue -- this needs to be evaluated.
  ok = false;
  (void) typeid (ok = true, b);
  if (!ok)
    return false;
  ok = false;
  (void) typeid (ok = true, d);
  return ok;
}

static_assert (test ());
