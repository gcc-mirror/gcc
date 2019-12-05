// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }
// Test basic use.

struct S {
  int i;
  constexpr S(bool b) {
    if (b)
      i = 42;
  }
};
constexpr S s1(true);
constexpr S s2(false); // { dg-error "not a constant expression" }

constexpr int
fn1 (int x)
{
  int a;
  a = 5;
  return x + a;
}

static_assert (fn1 (2) == 7);

constexpr int
fn2 (int x)
{
  const int a; // { dg-error "uninitialized .const a." }
  constexpr int b; // { dg-error "uninitialized .const b." }
  return x;
}

constexpr int
fn3 (int x)
{
  int a; // { dg-message ".int a. is not const" }
  return x + a; // { dg-error "the value of .a. is not usable in a constant expression" }
}

constexpr int a = fn3 (5); // { dg-message "in .constexpr. expansion of" }

constexpr int
fn4 ()
{
  struct S { int a = -5; int b; } s;
  return s.a;
}

static_assert (fn4 () == -5);

constexpr int
fn5 ()
{
  struct S { int a = 9; int b; } s;
  return s.b;
}

constexpr int b = fn5 (); // { dg-error "accessing uninitialized member" }
// { dg-message "in .constexpr. expansion of" "" { target *-*-* } .-1 }

constexpr int
fn6 ()
{
  int a;
  return 42;
}

static_assert (fn6 () == 42);

constexpr int
fn7 (bool b)
{
  int a; // { dg-message ".int a. is not const" }
  if (b)
    a = 42;
  return a;
}

static_assert (fn7 (true) == 42);
static_assert (fn7 (false) == 42); // { dg-error "non-constant condition|the value of .a. is not usable" }
// { dg-message "in .constexpr. expansion of" "" { target *-*-* } .-1 }

constexpr int
fn8 (int n)
{
  int r;
  switch (n)
    {
    case 1:
    r = n;
    return r;
    case 42:
    r = n;
    return r;
    }
}

static_assert (fn8 (1) == 1);
static_assert (fn8 (42) == 42);
