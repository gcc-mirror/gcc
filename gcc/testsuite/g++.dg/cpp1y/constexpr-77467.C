// PR c++/77467
// { dg-do compile { target c++14 } }

constexpr int
foo (const int x, const unsigned n) noexcept
{
  switch (n)
    {
    case 0:
      return 1;
    case 1:
      return x;
    default:
      const auto m = (n >> 1);
      const auto y = foo (x, m);
      return ((m << 1) == n) ? y * y : x * y * y;
    }
}

static_assert (foo (3, 2) == 9, "");
static_assert (foo (2, 3) == 8, "");

constexpr int
bar (int x)
{
  int a = x;
  switch (x)
    a = x + 1;
  return a;
}

static_assert (bar (0) == 0, "");
static_assert (bar (1) == 1, "");

constexpr int
baz (const int x, int y) noexcept
{
  switch (x)
    {
    case 0:
      return 1;
    case 1:
      return x;
    case 2:
      if ((y += 2) == 0)
	{
	case 3:
	  y += 4;
	  break;
	}
      else
	{
	case 4:
	  y += 8;
	  break;
	}
      break;
    case 5:
      for (y = 0; y < 3; y++)
	{
	case 7:
	  if (y == -4)
	    y += 3;
	  if (y == -3)
	    continue;
	  if (y == -2)
	    {
	      y += 18;
	      break;
	    }
	  if (y == 2)
	    {
	    case 6:
	      y += 12;
	    default:
	      y++;
	      break;
	    }
	}
      break;
    case -1:
    case -2:
      switch (y)
	{
	case 19:
	  y += 2;
	  break;
	case 20:
	  y += 3;
	  if (x == 2)
	    case 21:;
	  y += 2;
	  if (x == 3)
	    default:;
	  y += 4;
	  break;
	}
      return x + y + 1;
    }
  return x + y;
}

static_assert (baz (0, 7) == 1, "");
static_assert (baz (1, 7) == 1, "");
static_assert (baz (2, -2) == 6, "");
static_assert (baz (2, 0) == 12, "");
static_assert (baz (3, 1) == 8, "");
static_assert (baz (4, 2) == 14, "");
static_assert (baz (5, -20) == 20, "");
static_assert (baz (6, 5) == 24, "");
static_assert (baz (7, -5) == 22, "");
static_assert (baz (7, -4) == 22, "");
static_assert (baz (7, -3) == 23, "");
static_assert (baz (7, -2) == 23, "");
static_assert (baz (7, -1) == 22, "");
static_assert (baz (7, 0) == 22, "");
static_assert (baz (7, 2) == 22, "");
static_assert (baz (7, 6) == 14, "");
static_assert (baz (8, 9) == 18, "");
static_assert (baz (8, -2) == 7, "");
static_assert (baz (-1, 19) == 21, "");
static_assert (baz (-1, 20) == 29, "");
static_assert (baz (-1, 21) == 27, "");
static_assert (baz (-1, 5) == 9, "");
static_assert (baz (-2, 19) == 20, "");
static_assert (baz (-2, 20) == 28, "");
static_assert (baz (-2, 21) == 26, "");
static_assert (baz (-2, 5) == 8, "");
