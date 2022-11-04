// Test that s.i is not modified by the assume.
// { dg-do compile { target c++17 } }

struct string
{
  const char *p;
  int i;
  constexpr string (const char *p): p(p), i(0) { }
  constexpr int length () { ++i; return __builtin_strlen (p); }
};

constexpr int f()
{
  string s ("foobar");
  [[assume (s.length () > 0)]];
  if (s.i != 0) __builtin_abort();
  int len = s.length ();
  if (s.i != 1) __builtin_abort();
  return len;
}

static_assert (f());
