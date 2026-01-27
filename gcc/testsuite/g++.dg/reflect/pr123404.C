// PR c++/123404
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  decltype (^^::) a = ^^::;
  consteval S () {}
  S (const S &) = default;			// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  S (S &&) = default;				// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  S &operator= (const S &) = default;		// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  S &operator= (S &&) = default;		// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  consteval const char *what () { return "what"; }
};

template <typename T, T V>
struct U
{
  T a = V;
  consteval U () {}
  U (const U &) = default;			// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  U (U &&) = default;				// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  U &operator= (const U &) = default;		// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  U &operator= (U &&) = default;		// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
  consteval const char *what () { return "what"; }
};

consteval
{
  S s;
  S t;
  t = s;
  S u = t;
  u.what ();
  U <decltype (^^::), ^^::> v;
  U <decltype (^^::), ^^::> w;
  w = v;
  U <decltype (^^::), ^^::> x = w;
  x.what ();
}
