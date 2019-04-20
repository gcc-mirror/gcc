// PR c++/89833
// Test to verify that arrays of null pointer to members used as
// non-type template arguments are interprested as null regardless
// of the form of their initialization.
// { dg-do compile { target c++2a } }
// { dg-options "-O2 -Wall -fdump-tree-optimized" }

struct A { int i; };

typedef int A::*pam_t;

struct B { pam_t a[2]; };
template <B x> struct C { static constexpr B b = x; };

B f__   () { return B{ }; }
B f0_   () { return B{ 0 }; }
B f00   () { return B{ 0, 0 }; }

typedef C<B{ }>      X__;
typedef C<B{ 0 }>    X0_;
typedef C<B{ 0, 0 }> X00;

B g__ () { return X__::b; }
B g0_ () { return X0_::b; }
B g00 () { return X00::b; }

const B b__{ };
const B b0_{ 0 };
const B b00{ 0, 0 };

const pam_t apam__[2] = { };
const pam_t apam0_[2] = { 0 };
const pam_t apam00[2] = { 0, 0 };

#define assert(expr) \
  (expr) ? (void)0 : __builtin_abort ()

void test ()
{
  assert (f__ ().a[0] == nullptr && f__ ().a[1] == nullptr);
  assert (f0_ ().a[0] == nullptr && f0_ ().a[1] == nullptr);
  assert (f00 ().a[0] == nullptr && f00 ().a[1] == nullptr);

  assert (g__ ().a[0] == nullptr && g__ ().a[1] == nullptr);
  assert (g0_ ().a[0] == nullptr && g0_ ().a[1] == nullptr);
  assert (g00 ().a[0] == nullptr && g00 ().a[1] == nullptr);

  assert (b__.a[0] == nullptr && b__.a[1] == nullptr);
  assert (b0_.a[0] == nullptr && b0_.a[1] == nullptr);
  assert (b00.a[0] == nullptr && b00.a[1] == nullptr);

  assert (apam__[0] == nullptr && apam__[1] == nullptr);
  assert (apam0_[0] == nullptr && apam0_[1] == nullptr);
  assert (apam00[0] == nullptr && apam00[1] == nullptr);
}

// { dg-final { scan-tree-dump-not "abort" "optimized" } }
