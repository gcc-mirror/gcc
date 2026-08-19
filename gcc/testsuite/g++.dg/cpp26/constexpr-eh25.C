// PR c++/126918
// { dg-do compile { target c++26 } }

struct S {
  int m, n;
  int foo (int x) { return x + 42; }
  int bar (int x) noexcept { return x + 42; }
};

constexpr int S::*const &
foo (bool x)
{
  static constexpr int S::*sm = &S::m;
  if (x)
    return sm;
  try
    {
      throw nullptr;
    }
  catch (int S::*const &p)
    {
      return p;
    }
  return sm;
}

constexpr const int S::*const &
bar (bool x)
{
  static constexpr const int S::*np = nullptr;
  if (x)
    return np;
  try
    {
      throw &S::m;
    }
  catch (const int S::*const &p)
    {
      return p;
    }
  return np;
}

using F = int (S::*) (int);
using FNE = int (S::*) (int) noexcept;

constexpr F const &
baz (bool x)
{
  static constexpr F f = &S::foo;
  if (x)
    return f;
  try
    {
      throw nullptr;
    }
  catch (F const &p)
    {
      return p;
    }
  return f;
}

constexpr F const &
qux (bool x)
{
  static constexpr F np = nullptr;
  if (x)
    return np;
  try
    {
      FNE fne = &S::bar;
      throw fne;
    }
  catch (F const &p)
    {
      return p;
    }
  return np;
}

static_assert (foo (false) == nullptr);		// { dg-error "non-constant condition for static assertion" }
						// { dg-error "use of allocated storage after deallocation in a constant expression" "" { target *-*-* } .-1 }
static_assert (bar (false) == &S::m);		// { dg-error "non-constant condition for static assertion" }
						// { dg-error "use of allocated storage after deallocation in a constant expression" "" { target *-*-* } .-1 }
static_assert (baz (false) == nullptr);		// { dg-error "non-constant condition for static assertion" }
						// { dg-error "use of allocated storage after deallocation in a constant expression" "" { target *-*-* } .-1 }
static_assert (qux (false) == F (&S::bar));	// { dg-error "non-constant condition for static assertion" }
						// { dg-error "use of allocated storage after deallocation in a constant expression" "" { target *-*-* } .-1 }
