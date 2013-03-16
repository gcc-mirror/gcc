// PR c++/56607
// { dg-do compile { target { { lp64 || ilp32 } || llp64 } } }
// { dg-options "-O2 -Wdiv-by-zero -std=c++11" }

constexpr int sc () { return sizeof (char); }
constexpr int si () { return sizeof (int); }
constexpr int zc () { return sc () - 1; }
constexpr int zi (int d) { return si () / d - 1; }

int
f1 (void)
{
  return 1 / zc ();			// { dg-warning "division by zero" }
}

int
f2 (void)
{
  constexpr int x = zc ();
  return 1 / x;				// { dg-warning "division by zero" }
}

int
f3 (void)
{
  return 1 / zi (3);			// { dg-warning "division by zero" }
}

int
f4 (void)
{
  constexpr int x = zi (3);
  return 1 / x;				// { dg-warning "division by zero" }
}
