// PR c++/89513
// { dg-do compile { target c++14 } }
// { dg-options "" }

constexpr int foo ()
try {			// { dg-warning "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  int a = 1;
  for (int i = 0; i < 10; i++)
    a += i;
  return a;
} catch (...) {
  return -1;
}

constexpr int bar ()
try {			// { dg-warning "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  int a = 0;
  for (int i = 0; i < 9; i++)
    try {			// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
      a += i;
    } catch (int) {
      return -1;
    }
  return a;
} catch (...) {
  return -2;
}

constexpr bool baz ()
{
  try { return true; } catch (...) { return false; }	// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
}

struct S {
  constexpr S () try : m (1)	// { dg-warning "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  {
    try {		// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
      m += 2;
    } catch (int) {
      m = -1;
    }
  } catch (...) {
    m = -2;
  }
  int m;
  constexpr int get () const { return m; }
};

struct T {
  constexpr T ()
  try {			// { dg-warning "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  } catch (...) {
  }
};

static_assert (foo () == 46, "");
static_assert (bar () == 36, "");
static_assert (baz (), "");
constexpr S s;
static_assert (s.get () == 3, "");
constexpr T t;
