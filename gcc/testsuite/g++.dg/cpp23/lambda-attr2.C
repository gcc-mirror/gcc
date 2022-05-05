// P2173R1 - Attributes on Lambda-Expressions
// { dg-do compile { target c++11 } }

void
foo (bool x, bool y)
{
  auto i = [] [[ gnu::unavailable ]] () {};
  i ();			// { dg-error "'foo\\\(bool, bool\\\)::<lambda\\\(\\\)>' is unavailable" }
  auto j = [][[gnu::unavailable]] {};
  j ();			// { dg-error "'foo\\\(bool, bool\\\)::<lambda\\\(\\\)>' is unavailable" }
#if __cpp_generic_lambdas >= 201304
  auto k = [] [[gnu::unavailable]] (auto x) {};	// { dg-bogus "is unavailable" }
  k (0.0);		// { dg-error "'foo\\\(bool, bool\\\)::<lambda\\\(auto:1\\\)>\[^\n\r]*' is unavailable" "" { target c++14 } }
#endif
#if __cpp_generic_lambdas >= 201707
  auto l = [] <typename T> [[gnu::unavailable]] (T) {};	// { dg-bogus "is unavailable" }
  l (1);		// { dg-error "'foo\\\(bool, bool\\\)::<lambda\\\(T\\\)>\[^\n\r]*' is unavailable" "" { target c++20 } }
#endif
}
