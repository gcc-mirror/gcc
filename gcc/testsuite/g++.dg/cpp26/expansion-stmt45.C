// PR c++/125601
// { dg-do run { target c++14 } }
// { dg-options "-O2" }

constexpr int
foo (int x)
{
  int a = 0, b = 3;
  while (b > 0)
    {
      ++a;
      --b;
      template for (constexpr int value : { 10, 20, 30 })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
        {
          a += value;
	  if (x == 0)
	    break;
	  else if (x == 1)
	    continue;
	  a += 42;
        }
    }
  return a;
}

constexpr int
bar (int x)
{
  int a = 0;
  template for (constexpr int value : { 10, 20, 30 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      a += value;
      if (x == 0)
	break;
      else if (x == 1)
	continue;
      a += 42;
    }
  return a;
}

static_assert (foo (0) == 3 * (1 + 10), "");
static_assert (foo (1) == 3 * (1 + 10 + 20 + 30), "");
static_assert (foo (2) == 3 * (1 + 10 + 20 + 30 + 3 * 42), "");
static_assert (bar (0) == 10, "");
static_assert (bar (1) == 10 + 20 + 30, "");
static_assert (bar (2) == 10 + 20 + 30 + 3 * 42, "");

int
main ()
{
  if (foo (0) != 3 * (1 + 10))
    __builtin_abort ();
  if (foo (1) != 3 * (1 + 10 + 20 + 30))
    __builtin_abort ();
  if (foo (2) != 3 * (1 + 10 + 20 + 30 + 3 * 42))
    __builtin_abort ();
  if (bar (0) != 10)
    __builtin_abort ();
  if (bar (1) != 10 + 20 + 30)
    __builtin_abort ();
  if (bar (2) != 10 + 20 + 30 + 3 * 42)
    __builtin_abort ();
}
