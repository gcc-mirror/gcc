// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

constexpr char p[] = "hello";
constexpr const char *q[] = { &p[0], &p[3] };
constexpr const char *const *r = &q[0];
const char *s[] = { &p[0], &p[3] };
constexpr const char **t = &s[0];

constexpr bool
foo ()
{
  try
    {
      throw t;
    }
  catch (const char **const &x)
    {
      if (x != t)
	return false;
      try
	{
	  throw;
	}
      catch (const char **&y)
	{
	  if (y != t)
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (const char **z)
	    {
	      if (z != t)
		return false;
	      try
		{
		  throw;
		}
	      catch (const char *const *const &v)
		{
		  if (v != (const char *const *) t)
		    return false;
		  try
		    {
		      throw;
		    }
		  catch (const char *const *w)
		    {
		      if (w != (const char *const *) t)
			return false;
		      return true;
		    }
		}
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
      throw nullptr;
    }
  catch (const char **const &x)
    {
      if (x != nullptr)
	return false;
      try
	{
	  throw;
	}
      catch (const char **&y)
	{
	  if (y != nullptr)
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (const char **z)
	    {
	      if (z != nullptr)
		return false;
	      try
		{
		  throw;
		}
	      catch (const char *const *const &v)
		{
		  if (v != nullptr)
		    return false;
		  try
		    {
		      throw;
		    }
		  catch (const char *const *w)
		    {
		      if (w != nullptr)
			return false;
		      return true;
		    }
		}
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
      throw r;
    }
  catch (const char *const *const &x)
    {
      if (x != r || **x != 'h')
	return false;
      try
	{
	  throw;
	}
      catch (const char *const *&y)
	{
	  if (y != r || **y != 'h')
	    return false;
	  try
	    {
	      throw;
	    }
	  catch (const char *const *z)
	    {
	      if (z != r || **z != 'h')
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
