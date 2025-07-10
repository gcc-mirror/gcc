// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

namespace std
{
  struct exception
  {
    constexpr exception () noexcept { }
    constexpr virtual ~exception () noexcept {}
    constexpr exception (const exception &) = default;
    constexpr exception &operator= (const exception &) = default;
    constexpr exception (exception &&) = default;
    constexpr exception &operator= (exception &&) = default;
    constexpr virtual const char *what () const noexcept
    { return "std::exception"; }
  };
}

struct S : public std::exception {
  constexpr S () : s (0) {}
  constexpr S (int x) : s (x) {}
  constexpr S (const S &x) : s (x.s) {}
  constexpr virtual ~S () {}
  constexpr virtual const char *what () noexcept { return "this is S"; }
  int s;
};
struct T : public std::exception {
  constexpr T () : s (new char[1]), t (0) { s[0] = '\0'; }
  constexpr T (const char *p, int q) : s (new char[q + 1]), t (q)
  {
    for (int i = 0; i <= t; ++i)
      s[i] = p[i];
  }
  constexpr T (const T &x) : s (new char[x.t + 1]), t (x.t)
  {
    for (int i = 0; i <= t; ++i)
      s[i] = x.s[i];
  }
  constexpr virtual ~T () { delete[] s; }
  constexpr virtual const char *what () noexcept { return s; }
  char *s;
  int t;
};
struct U {
  constexpr U () : x (0), y (0), z (0) {}
  constexpr U (int a, long b, unsigned long long c) : x (a), y (b), z (c) {}
  constexpr U (const U &u) = default;
  int x;
  long y;
  unsigned long long z;
};
struct V {
  constexpr V () : v (0) {}
  constexpr V (int x) : v (x) {}
  constexpr V (const V &x) : v (x.v) {}
  constexpr virtual ~V () {}
  constexpr virtual const char *what () noexcept { return "this is V"; }
  int v;
};

constexpr int
foo (int x)
{
  if (x == 1)
    throw S (42);
  else if (x == 2)
    throw T ("hello, world", sizeof ("hello, world") - 1);
  else if (x == 3)
    throw U (1, -2L, 42ULL);
  else if (x == 4)
    throw 42;
  else if (x == 5)
    throw 1.0;
  else if (x == 6)
    throw V (42);
  else
    return 42;
}

constexpr int
bar (int x) noexcept
// { dg-error "'std::terminate' called after throwing an exception of type 'S'; 'what\\\(\\\)': 'this is S'" "" { target *-*-* } .-1 }
// { dg-message "uncaught exception exited from 'noexcept' function 'constexpr int bar\\\(int\\\)'" "" { target *-*-* } .-2 }
// { dg-error "'std::terminate' called after throwing an exception of type 'T'; 'what\\\(\\\)': 'hello, world'" "" { target *-*-* } .-3 }
// { dg-error "'std::terminate' called after throwing an exception 'U\\\{1, -2, 42\\\}'" "" { target *-*-* } .-4 }
// { dg-error "'std::terminate' called after throwing an exception '42'" "" { target *-*-* } .-5 }
// { dg-error "'std::terminate' called after throwing an exception '1\\\.0e\\\+0'" "" { target *-*-* } .-6 }
// { dg-error "'std::terminate' called after throwing an exception 'V\\\{\[^\n\r]*42\\\}" "" { target *-*-* } .-7 }
{
  return foo (x);
}

constexpr int
baz (int x)
{
  try
    {
      return foo (x);
    }
  catch (...)
    {
      return -1;
    }
}

static_assert (bar (0) == 42);
constexpr int a = bar (1);		// { dg-message "in 'constexpr' expansion of" }
constexpr int b = bar (2);		// { dg-message "in 'constexpr' expansion of" }
constexpr int c = bar (3);		// { dg-message "in 'constexpr' expansion of" }
constexpr int d = bar (4);		// { dg-message "in 'constexpr' expansion of" }
constexpr int e = bar (5);		// { dg-message "in 'constexpr' expansion of" }
constexpr int f = bar (6);		// { dg-message "in 'constexpr' expansion of" }
static_assert (baz (0) == 42);
static_assert (baz (1) == -1);
static_assert (baz (2) == -1);
static_assert (baz (3) == -1);
static_assert (baz (4) == -1);
static_assert (baz (5) == -1);
static_assert (baz (6) == -1);
static_assert (foo (0) == 42);
constexpr int g = foo (1);		// { dg-error "uncaught exception of type 'S'; 'what\\\(\\\)': 'this is S'" }
constexpr int h = foo (2);		// { dg-error "uncaught exception of type 'T'; 'what\\\(\\\)': 'hello, world'" }
constexpr int i = foo (3);		// { dg-error "uncaught exception 'U\\\{1, -2, 42\\\}'" }
constexpr int j = foo (4);		// { dg-error "uncaught exception '42'" }
constexpr int k = foo (5);		// { dg-error "uncaught exception '1\\\.0e\\\+0'" }
constexpr int l = foo (6);		// { dg-error "uncaught exception 'V\\\{\[^\n\r]*42\\\}'" }
