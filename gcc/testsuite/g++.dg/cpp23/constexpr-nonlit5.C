// { dg-do compile }
// { dg-options "-std=c++2b" }

int qux ();

constexpr int
foo (int x)
{
  switch (x)
    {
      static const int v = 6;
    case 12:
      return v;
    }
  return 0;
}

constexpr int
bar (int x)
{
  switch (x)
    {
      thread_local const int v = 7;
    case 12:
      return 7;
    }
  return 0;
}

constexpr int
baz (int x)
{
  switch (x)
    {
      static int v = 6;	// { dg-message "int v' is not const" }
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
      thread_local int v = 6;	// { dg-message "int v' is not const" }
    case 12:
      return v;	// { dg-error "the value of 'v' is not usable in a constant expression" }
    }
  return 0;
}

constexpr int a = foo (12);
constexpr int b = bar (12);
constexpr int c = baz (12);
constexpr int d = corge (12);
