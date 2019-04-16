// PR c++/88983
// { dg-do compile { target c++14 } }

constexpr int
fn1 (int ay)
{
  switch (ay)
    {
      if (1)
        {
          case 1:
            return 1;
        }
      else
        {
          default:;
        }
    }

  return 0;
}

constexpr int
fn2 (int ay)
{
  switch (ay)
    {
      if (1)
        {
          case 1:
	    break;
        }
      else
        {
          default:;
        }
    }

  return 0;
}

constexpr int
fn3 (int ay)
{
  int i = 0;
  while (i++ < 100)
    {
      if (i == 1)
	return 1;
      switch (ay)
	{
	  if (1)
	    {
	      case 1:
		continue;
	    }
	  else
	    {
	      default:;
	      return -1;
	    }
	}
      return -1;
    }

  return -1;
}

static_assert (fn1 (1) == 1, "");
static_assert (fn2 (1) == 0, "");
static_assert (fn3 (1) == 1, "");
