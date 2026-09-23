// PR c++/126918
// { dg-do compile { target c++26 } }

struct S {
  int m, n;
  int foo (int x) { return x + 42; }
  int bar (int x) noexcept { return x + 42; }
};

constexpr int S::*
foo (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw nullptr;
    }
  catch (int S::*p)
    {
      return p;
    }
  return &S::m;
}

constexpr int S::*
bar (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw nullptr;
    }
  catch (int S::*const &p)
    {
      return p;
    }
  return &S::m;
}

constexpr const int S::*
baz (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw &S::m;
    }
  catch (const int S::*p)
    {
      return p;
    }
  return nullptr;
}

constexpr const int S::*
qux (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw &S::m;
    }
  catch (const int S::*const &p)
    {
      return p;
    }
  return nullptr;
}

using F = int (S::*) (int);
using FNE = int (S::*) (int) noexcept;

constexpr F
corge (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw nullptr;
    }
  catch (F p)
    {
      return p;
    }
  return &S::foo;
}

constexpr F
garply (bool x)
{
  if (x)
    return nullptr;
  try
    {
      throw nullptr;
    }
  catch (F const &p)
    {
      return p;
    }
  return &S::foo;
}

constexpr F
fred (bool x)
{
  if (x)
    return nullptr;
  try
    {
      FNE fne = &S::bar;
      throw fne;
    }
  catch (F p)
    {
      return p;
    }
  return nullptr;
}

constexpr F
waldo (bool x)
{
  if (x)
    return nullptr;
  try
    {
      FNE fne = &S::bar;
      throw fne;
    }
  catch (F const &p)
    {
      return p;
    }
  return nullptr;
}

static_assert (foo (false) == nullptr);
static_assert (bar (false) == nullptr);
static_assert (baz (false) == &S::m);
static_assert (qux (false) == &S::m);
static_assert (corge (false) == nullptr);
static_assert (garply (false) == nullptr);
static_assert (fred (false) == F (&S::bar));
static_assert (waldo (false) == F (&S::bar));

int
main ()
{
  if (foo (false) != nullptr
      || bar (false) != nullptr
      || baz (false) != &S::m
      || qux (false) != &S::m
      || corge (false) != nullptr
      || garply (false) != nullptr
      || fred (false) != F (&S::bar)
      || waldo (false) != F (&S::bar))
    __builtin_abort ();
}
