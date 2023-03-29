// Diagnose failed assumptions involving a function call.
// { dg-do compile { target c++17 } }

struct string
{
  const char *p;
  constexpr string (const char *p): p(p) { }
  constexpr int length () { return __builtin_strlen (p); }
};

constexpr int f()
{
  string s ("foobar");
  [[assume (s.length () == 0)]]; // { dg-error "assume" }
  // { dg-message "6 == 0" "" { target *-*-* } .-1 }
  return s.length ();
}

static_assert (f());		// { dg-error "non-constant" }
