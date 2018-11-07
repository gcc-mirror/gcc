// PR c++/86767
// { dg-do compile { target c++14 } }

constexpr int
fn0 () noexcept
{
  int r = 0;
  for (int i = 0; i < 10; ++i)
    {
      continue;
      r++;
      for (int j = 0; j < 10; ++j )
	{
	}
    }
  return r;
}
static_assert (fn0 () == 0, "");

constexpr int
fn1 () noexcept
{
  int r = 0;
  for (int i = 0; i < 10; ++i)
    for (int j = 0; j < 10; ++j)
      {
	continue;
	r++;
      }
  return r;
}
static_assert (fn1 () == 0, "");

constexpr int
fn2 () noexcept
{
  int r = 0;
  for (int i = 0; i < 10; ++i)
    {
      continue;
      r++;
    }
  return r;
}
static_assert (fn2 () == 0, "");

constexpr int
fn3 () noexcept
{
  int r = 0;
  for (int i = 0; i < 10; ++i)
    {
      continue;
      r++;
      while (1)
	{
	}
    }
  return r;
}
static_assert (fn3 () == 0, "");

constexpr int
fn4 () noexcept
{
  for (int i = 0; i < 10; ++i)
    {
      switch (i)
	{
	case 5:
	  return i;
	default:
	  continue;
	}
      while (1)
	{
	}
    }
  return 0;
}
static_assert (fn4 () == 5, "");

constexpr int
fn5 () noexcept
{
  for (int i = 0; i < 10; ++i)
    {
      switch (i)
	{
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	  continue;
	default:
	  return i;
	}
      while (1)
	{
	}
    }
  return 0;
}
static_assert (fn5 () == 5, "");

constexpr int
fn6 () noexcept
{
  int r = 0;
  for (int i = 0; i < 10; ++i)
    {
      continue;
      for (int j = 0; j < 10; ++j )
	r++;
    }
  return r;
}
static_assert (fn6 () == 0, "");
