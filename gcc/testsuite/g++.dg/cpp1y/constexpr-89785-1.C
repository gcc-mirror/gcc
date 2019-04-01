// PR c++/89785
// { dg-do compile { target c++14 } }

constexpr int
foo (int x)
{
  switch (x)
    {
    case 0:
      throw -42;
    case 2:
      return 42;
    }
  throw 42;
}

constexpr int
bar (int x)
{
  do
    {
      switch (x)
	{
	case 0:
	  throw 42;
	case 1:
	  continue;
	}
      throw -42;
    }
  while (0);
  return x;
}

static_assert (foo (2) == 42, "");
static_assert (bar (1) == 1, "");
