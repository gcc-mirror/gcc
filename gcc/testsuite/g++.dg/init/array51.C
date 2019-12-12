// PR c++/89833
// Anal test to verify that arrays of null pointer to members are
// treated as null regardless of the form of their initialization,
// and have all bits set in their representation.
// { dg-do run { target c++11 } }
// { dg-options "-O2 -Wall" }

#define NOIPA __attribute__ ((noipa))

struct A { int i; };

typedef int A::*pam_t;

pam_t apam__[2] = { };
pam_t apam0_[2] = { 0 };
pam_t apam00[2] = { 0, 0 };

struct B { pam_t a[2]; };

NOIPA B f__   () { return B{ }; }
NOIPA B f0_   () { return B{ 0 }; }
NOIPA B f00   () { return B{ 0, 0 }; }

const B c__{ };
const B c0_{ 0 };
const B c00{ 0, 0 };

B b__{ };
B b0_{ 0 };
B b00{ 0, 0 };

#define assert(expr)				\
  (expr) ? (void)0 : __builtin_abort ()

signed char allones[2 * sizeof (pam_t)];

#define assert_rep(mp, n)			\
  assert (!test_allones (mp, n))

NOIPA void init_allones ()
{
  __builtin_memset (allones, -1, sizeof allones);
}

NOIPA int test_allones (const pam_t *p, unsigned n)
{
  return __builtin_memcmp (allones, p, sizeof *p * n);
}

int main ()
{
  init_allones ();

  assert (apam__[0] == nullptr && apam__[1] == nullptr);
  assert (apam0_[0] == nullptr && apam0_[1] == nullptr);
  assert (apam00[0] == nullptr && apam00[1] == nullptr);

  assert (f__ ().a[0] == nullptr && f__ ().a[1] == nullptr);
  assert (f0_ ().a[0] == nullptr && f0_ ().a[1] == nullptr);
  assert (f00 ().a[0] == nullptr && f00 ().a[1] == nullptr);

  assert (b__.a[0] == nullptr && b__.a[1] == nullptr);
  assert (b0_.a[0] == nullptr && b0_.a[1] == nullptr);
  assert (b00.a[0] == nullptr && b00.a[1] == nullptr);

  assert (c__.a[0] == nullptr && c__.a[1] == nullptr);
  assert (c0_.a[0] == nullptr && c0_.a[1] == nullptr);
  assert (c00.a[0] == nullptr && c00.a[1] == nullptr);

  assert_rep (apam__, 2);
  assert_rep (apam0_, 2);
  assert_rep (apam00, 2);

  assert_rep (f__ ().a, 2);
  assert_rep (f0_ ().a, 2);
  assert_rep (f0_ ().a, 2);
  assert_rep (f00 ().a, 2);

  assert_rep (b__.a, 2);
  assert_rep (b0_.a, 2);
  assert_rep (b00.a, 2);

  assert_rep (c__.a, 2);
  assert_rep (c0_.a, 2);
  assert_rep (c00.a, 2);
}
