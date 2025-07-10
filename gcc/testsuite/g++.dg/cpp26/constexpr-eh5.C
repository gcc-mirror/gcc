// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

constexpr void
foo ()
{
  throw 1;
}

void
bar ()
{
}

constexpr void
baz ()
{
  foo ();
  bar ();
}

constexpr void
qux ()
{
  if consteval {
    throw 2;
  }
  bar ();
}

constexpr bool
corge ()
{
  try
    {
      baz ();
    }
  catch (int a)
    {
      if (a != 1)
	return false;
      try
	{
	  qux ();
	}
      catch (int b)
	{
	  return b == 2;
	}
    }
  return false;
}

static_assert (corge ());
