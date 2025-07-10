// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct S {
  constexpr S () : s (0) {}
  constexpr S (int x) : s (x) {}
  constexpr S (const S &x) : s (x.s) {}
  constexpr ~S () {}
  int s;
};
struct T {
  constexpr T () : t (0) {}
  constexpr T (int x) : t (x) {}
  constexpr T (const T &x) : t (x.t) {}
  constexpr ~T () {}
  int t;
};
struct U : public S, public T {
  constexpr U () : S (0), T (0) {}
  constexpr U (int x, int y) : S (x), T (y) {}
  constexpr U (const U &x) : S (x.s), T (x.t) {}
  constexpr ~U () {}
};

constexpr bool
foo ()
{
  try
    {
      throw U (1, 2);
    }
  catch (const U &x)
    {
      if (x.s != 1 || x.t != 2)
	return false;
      try
	{
	  throw;
	}
      catch (const S &y)
	{
	  if (y.s != 1)
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (const T &z)
	    {
	      if (z.t != 2)
		return false;
	      return true;
	    }
	}
    }
  return false;
}

constexpr bool
bar ()
{
  try
    {
      throw U (1, 2);
    }
  catch (U &x)
    {
      if (x.s != 1 || x.t != 2)
	return false;
      try
	{
	  x.s = 3;
	  x.t = 4;
	  throw;
	}
      catch (S &y)
	{
	  if (y.s != 3)
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (T &z)
	    {
	      if (z.t != 4)
		return false;
	      return true;
	    }
	}
    }
  return false;
}

constexpr bool
baz ()
{
  try
    {
      throw U (1, 2);
    }
  catch (U x)
    {
      if (x.s != 1 || x.t != 2)
	return false;
      try
	{
	  x.s = 3;
	  x.t = 4;
	  throw;
	}
      catch (S y)
	{
	  if (y.s != 1)
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (T z)
	    {
	      if (z.t != 2)
		return false;
	      return true;
	    }
	}
    }
  return false;
}

static_assert (foo ());
static_assert (bar ());
static_assert (baz ());
