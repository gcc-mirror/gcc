// PR c++/126343
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
namespace std {
  namespace meta {
    using info = decltype (^^::);
  }
  using nullptr_t = decltype (nullptr);
}
namespace my {
  using info = decltype (^^::) const;
  using nullptr_t = volatile decltype (nullptr);
}
constexpr auto a = ^^decltype (^^int);
constexpr auto b = ^^decltype (nullptr);
constexpr auto c = ^^const decltype (^^int);
constexpr auto d = ^^const decltype (nullptr);
constexpr auto e = ^^decltype (^^int) const volatile;
constexpr auto f = ^^decltype (nullptr) volatile;
constexpr auto g = ^^std::meta::info;
constexpr auto h = ^^std::nullptr_t;
constexpr auto i = ^^my::info;
constexpr auto j = ^^my::nullptr_t;
static_assert (a == ^^char);		// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(\\\^\\\^int\\\) == \\\^\\\^char\\\)'" "" { target *-*-* } .-1 }
static_assert (b == ^^short);		// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(nullptr\\\) == \\\^\\\^short int\\\)'" "" { target *-*-* } .-1 }
static_assert (c == ^^int);		// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(\\\^\\\^int\\\) const == \\\^\\\^int\\\)'" "" { target *-*-* } .-1 }
static_assert (d == ^^long);		// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(nullptr\\\) const == \\\^\\\^long int\\\)'" "" { target *-*-* } .-1 }
static_assert (e == ^^long long);	// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(\\\^\\\^int\\\) const volatile == \\\^\\\^long long int\\\)'" "" { target *-*-* } .-1 }
static_assert (f == ^^unsigned char);	// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^decltype\\\(nullptr\\\) volatile == \\\^\\\^unsigned char\\\)'" "" { target *-*-* } .-1 }
static_assert (g == ^^unsigned short);	// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^std::meta::info == \\\^\\\^short unsigned int\\\)'" "" { target *-*-* } .-1 }
static_assert (h == ^^unsigned int);	// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^std::nullptr_t == \\\^\\\^unsigned int\\\)'" "" { target *-*-* } .-1 }
static_assert (i == ^^unsigned long);	// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^my::info == \\\^\\\^long unsigned int\\\)'" "" { target *-*-* } .-1 }
static_assert (j == ^^unsigned long long);// { dg-error "static assertion " }
					// { dg-message "note: the comparison reduces to '\\\(\\\^\\\^my::nullptr_t == \\\^\\\^long long unsigned int\\\)'" "" { target *-*-* } .-1 }
