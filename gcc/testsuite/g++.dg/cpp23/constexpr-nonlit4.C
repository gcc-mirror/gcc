// { dg-do compile }
// { dg-options "-std=c++2b" }

int qux ();

constexpr int
foo (int x)
{
  switch (x)
    {
      static int v = qux ();
    case 12:
      return 1;
    }
  return 0;
}

constexpr int
bar (int x)
{
  switch (x)
    {
      thread_local int v = qux ();
    case 12:
      return 1;
    }
  return 0;
}

constexpr int
baz (int x)
{
  switch (x)
    {
      static const int v = qux ();	// { dg-message "'v' was not initialized with a constant expression" }
    case 12:
      return v;	// { dg-error "the value of 'v' is not usable in a constant expression" }
    }
  return 0;
}

constexpr int
corge (int x)
{
  switch (x)
    {
      const thread_local int v = qux ();	// { dg-message "'v' was not initialized with a constant expression" }
    case 12:
      return v; // { dg-error "the value of 'v' is not usable in a constant expression" }
    }
  return 0;
}

constexpr int a = foo (12);
constexpr int b = bar (12);
constexpr int c = baz (12);
constexpr int d = corge (12);
