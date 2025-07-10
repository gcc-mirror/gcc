// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

int
foo (int x, int y)
{
  return x + y;
}

constexpr int
bar (int x)
{
  if (x < 0)
    throw x;
  return x;
}

constexpr int
baz (int x, int y)
{
  return foo (bar (x), bar (y));
}

constexpr int
qux (int x, int y)
{
  try
    {
      return baz (x, y);
    }
  catch (int)
    {
      return 42;
    }
}

static_assert (qux (12, -1) == 42);
static_assert (qux (-7, 12) == 42);
