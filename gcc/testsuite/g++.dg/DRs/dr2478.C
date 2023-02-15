// DR 2478 - Properties of explicit specializations of implicitly-instantiated class templates
// { dg-do compile { target c++20 } }

template <typename T>
struct S {
  int foo () { return 0; }
  constexpr int bar () { return 0; }
  int baz () { return 0; }
  consteval int qux () { return 0; }
  constexpr S () {}
  static constinit T x;
  static T y;
};

template <typename T>
T S<T>::x = S<T> ().foo ();	// { dg-error "'constinit' variable 'S<char>::x' does not have a constant initializer" }
				// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }

template <typename T>
T S<T>::y = S<T> ().foo ();

template <>
constexpr int
S<int>::foo ()
{
  return 0;
}

template <>
int
S<int>::bar ()
{
  return 0;
}

template <>
consteval int
S<char>::baz ()
{
  return 0;
}

template <>
int
S<char>::qux ()
{
  return 0;
}

template <>
long S<long>::x = S<long> ().foo ();	// { dg-bogus "'constinit' variable 'S<long int>::x' does not have a constant initializer" "" { xfail *-*-* } }
					// { dg-bogus "call to non-'constexpr' function" "" { xfail *-*-* } .-1 }

template <>
constinit long S<long>::y = S<long> ().foo ();	// { dg-error "'constinit' variable 'S<long int>::y' does not have a constant initializer" }
						// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }

constinit auto a = S<char> ().foo ();	// { dg-error "'constinit' variable 'a' does not have a constant initializer" }
					// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }
constinit auto b = S<char> ().bar ();
constinit auto c = S<int> ().foo ();
constinit auto d = S<int> ().bar ();	// { dg-error "'constinit' variable 'd' does not have a constant initializer" }
					// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }
constinit auto e = S<char> ().baz ();
constinit auto f = S<char> ().qux ();	// { dg-error "'constinit' variable 'f' does not have a constant initializer" }
					// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }
constinit auto g = S<int> ().baz ();	// { dg-error "'constinit' variable 'g' does not have a constant initializer" }
					// { dg-error "call to non-'constexpr' function" "" { target *-*-* } .-1 }
constinit auto h = S<int> ().qux ();
auto i = S<char>::x;
auto j = S<int>::x;
auto k = S<long>::x;
auto l = S<char>::y;
auto m = S<int>::y;
