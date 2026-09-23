// PR c++/126508
// { dg-do compile { target c++26 } }

constexpr int
foo ()
{
  try
    {
      throw;
    }
  catch (const int x)
    {
      return x;
    }
}

constexpr int
bar (int x)
{
  try
    {
      throw x;
    }
  catch (...)
    {
      return foo ();
    }
}

static_assert (bar (42) == 42);
static_assert (bar (43) == 43);
