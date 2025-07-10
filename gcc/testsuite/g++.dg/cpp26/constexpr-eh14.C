// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

template <typename T>
constexpr T
foo (T x, auto... y)
{
  const T z[] = { x, y... };
  try
    {
      throw z;
    }
  catch (const T (&a)[4])
    {
      return T ();
    }
  catch (const T *b)
    {
      return b[0];
    }
  catch (...)
    {
      return T ();
    }
}

void
bar ()
{
}

void
baz ()
{
}

static_assert (foo (42, 43, 44, 45, 46) == 42);
static_assert (foo (43U, 44U, 45U, 46U) == 43U);
static_assert (foo (44LL, 45LL) == 44LL);
static_assert (foo (bar, baz, bar, baz) == bar);
static_assert (foo (baz, bar) == baz);
