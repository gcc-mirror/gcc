// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable -Wunused-but-set-variable -Wunused-parameter -Wshadow" }

int a[3];

void
foo ()
{
  {
    int _ = 1;
    ++_;
  }
  {
    int _ = 3;
    ++_;
    int _ = 4;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
  {
    int _ = 5;
    --_;
    int _ = 6;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    int _ = 7;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
  {
    auto [i, j, _] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    ++i;
    ++_;
  }
  {
    auto [_, _, k] = a;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++k;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  }
  {
    auto [i, j, _] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    auto [_, k, l] = a;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++i;			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
    ++l;
  }
  {
    int _;
    _ = 1;
  }
  {
    int _ = 1;
  }
  {
    int _;
  }
  {
    static int _;		// { dg-warning "unused variable" }
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
  {
    extern int _ (int);
    extern long _ (long);
    extern float _ (float);
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
  {
    extern double _ (double);
    extern short _ (short);
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    int _ = 2;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
  {
    int _ = 1;
    {
      int _ = 2;
      ++_;
    }
    {
      static int _ = 3;
      ++_;
    }
    {
      auto [i, j, _] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
      ++_;
    }
  }
}

int
bar (int _ = 0)			// { dg-warning "unused parameter '_'" }
{
  int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  return 0;
}

void
baz ()
{
  if (int _ = bar ())
    int _ = 2;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  else
    int _ = 3;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  while (int _ = bar ())
    int _ = 4;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  for (int _ = bar (); _; ++_)
    int _ = 5;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  if (int _ = bar ())
    {
      int _ = 6;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
  else
    {
      int _ = 7;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
  while (int _ = bar ())
    {
      int _ = 8;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
  for (int _ = bar (); _; ++_)
    {
      int _ = 9;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    }
}

void
qux (short _ = 0)		// { dg-warning "unused parameter '_'" }
{
  {
    long _ = 1;
  }
}

void
corge ()
{
  auto b = [_ = 1] () { (void) _; };	// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } }
				// { dg-warning "variable 'b' set but not used" "" { target *-*-* } .-1 }
  auto c = [_ = 2, _ = 3] () {};// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } .-1 }
				// { dg-warning "variable 'c' set but not used" "" { target *-*-* } .-2 }
  {
    int _ = 4;
    auto d = [_, _ = 5] () {};	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }				// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } .-1 }
				// { dg-warning "variable 'd' set but not used" "" { target *-*-* } .-2 }
  {
    int _ = 5;
    auto e = [_ = 6] () {};	// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } }
  }				// { dg-warning "variable 'e' set but not used" "" { target *-*-* } .-1 }
}

namespace A {
  int _ = 11;
}

void
garply (int x,			// { dg-warning "unused parameter 'x'" }
	int _,			// { dg-warning "unused parameter '_'" }
	int)
{
}

void
fred ()
{
  try {
  } catch (int _) {
    int _ = 5;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }
}

void
waldo (int _)			// { dg-warning "unused parameter '_'" }
try
{
}
catch (int _)			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
{
  int _ = 7;
}

void
grault (int _)			// { dg-warning "unused parameter '_'" }
try
{
}
catch (int)
{
  int _ = 8;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
}

void
plugh (int _)			// { dg-warning "unused parameter '_'" }
try
{
  int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
}
catch (int)
{
}
