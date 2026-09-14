// Test that s.i is not modified by assume or a nested assume.
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

// PR c++/127282, a nested assume also needs to not modify values or allow
// other expressions within the enclosing assume to modify values.
constexpr int g()
{
  string s ("foobar");
  [[assume (
      [&](){[[assume (s.length () > 0)]]; return true; }()
   && s.length () > 0)
  ]];
  if (s.i != 0) __builtin_abort();
  int len = s.length ();
  if (s.i != 1) __builtin_abort();
  return len;
}

static_assert (g());
