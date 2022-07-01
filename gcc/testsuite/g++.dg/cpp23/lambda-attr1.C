// P2173R1 - Attributes on Lambda-Expressions
// { dg-do compile { target c++11 } }

void
foo (bool x, bool y)
{
  auto a = [][[noreturn]] () {};	// { dg-warning "'noreturn' function does return" }
  if (x)
    a ();
  auto b = [][[noreturn]] {};		// { dg-warning "'noreturn' function does return" }
  if (y)
    b ();
  auto c = [] [[ deprecated ]] () {};	// { dg-bogus "is deprecated" }
  c ();					// { dg-warning "'foo\\\(bool, bool\\\)::<lambda\\\(\\\)>' is deprecated" }
  auto d = [][[deprecated]] {};		// { dg-bogus "is deprecated" }
  d ();					// { dg-warning "'foo\\\(bool, bool\\\)::<lambda\\\(\\\)>' is deprecated" }
#if __cpp_generic_lambdas >= 201304
  auto e = [] [[deprecated]] (auto x) {};	// { dg-bogus "is deprecated" }
  e (0.0);				// { dg-warning "'foo\\\(bool, bool\\\)::<lambda\\\(auto:1\\\)>\[^\n\r]*' is deprecated" "" { target c++14 } }
#endif
#if __cpp_generic_lambdas >= 201707
  auto f = [] <typename T> [[deprecated]] (T) {};	// { dg-bogus "is deprecated" }
  f (1);				// { dg-warning "'foo\\\(bool, bool\\\)::<lambda\\\(T\\\)>\[^\n\r]*' is deprecated" "" { target c++20 } }
#endif
  auto g = [][[nodiscard]](int) { return 1; };
  g (1);				// { dg-warning "ignoring return value of 'foo\\\(bool, bool\\\)::<lambda\\\(int\\\)>', declared with attribute 'nodiscard'" }
  auto h = [] [[nodiscard]] { return 0; };
  h ();					// { dg-warning "ignoring return value of 'foo\\\(bool, bool\\\)::<lambda\\\(\\\)>', declared with attribute 'nodiscard'" }
  auto i = [] [[ gnu::unavailable ]] () {};
  auto j = [][[gnu::unavailable]] {};
#if __cpp_generic_lambdas >= 201304
  auto k = [] [[gnu::unavailable]] (auto x) {};	// { dg-bogus "is unavailable" }
#endif
#if __cpp_generic_lambdas >= 201707
  auto l = [] <typename T> [[gnu::unavailable]] (T) {};	// { dg-bogus "is unavailable" }
#endif
}
