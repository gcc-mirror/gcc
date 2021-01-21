// PR c++/98672
// { dg-do compile { target c++14 } }

void
foo ()
{
}

constexpr int
bar ()
{
  for (int i = 0; i < 5; ++i)
    return i;
  foo ();
  return 0;
}

constexpr int
baz ()
{
  int i = 0;
  while (i < 5)
    {
      if (i == 3)
	return i;
      else
	++i;
    }
  foo ();
  return 0;
}

constexpr int
qux (int x)
{
  if (x > 10)
    ++x;
  else
    return 7;
  foo ();
  return 0;
}

constexpr int
corge (int x)
{
  for (int a = 1; ; a++)
    {
      if (x > 10)
	++x;
      else
	return 4;
      foo ();
    }
}

constexpr int
garply (int x)
{
  for (int a = 1; ; a++)
    {
      if (x > 10)
	++x;
      else
	break;
      foo ();
    }
  return x;
}

constexpr int
waldo (int x)
{
  for (int a = 1; ; a++)
    {
      if (x > 10)
	break;
      else
	return 5;
      foo ();
    }
  foo ();
  return x;
}

constexpr int i = bar ();
constexpr int j = baz ();
constexpr int k = qux (4);
constexpr int l = corge (5);
constexpr int m = garply (2);
constexpr int n = waldo (-2);
static_assert (i == 0 && j == 3 && k == 7 && l == 4 && m == 2 && n == 5, "");
