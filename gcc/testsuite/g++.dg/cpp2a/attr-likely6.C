// PR c++/92343
// { dg-do compile { target c++14 } }

constexpr bool
foo (bool x)
{
  if (x)
    [[unlikely]] return true;
  else
    [[likely]] return false;
}

static_assert (foo (true), "");
static_assert (!foo (false), "");
