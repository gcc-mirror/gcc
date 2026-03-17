// { dg-do compile { target c++26 } }
// { dg-skip-if "" { implicit_constexpr } }

struct A {
  constexpr const char *data () const { return "foo"; }
};
struct B {
  constexpr decltype (sizeof 0) size () const { return 3; }
};
struct C {
  constexpr const char *data () const { return "bar"; }
  decltype (sizeof 0) size () const { return 3; }
};
struct D {
  const char *data () const { return "bar"; }
  constexpr decltype (sizeof 0) size () const { return 3; }
};
struct E {};
struct F {
  constexpr const char *data () const { return "bar"; }
  constexpr decltype (sizeof 0) size () const { return 3; }
};
struct G {
  constexpr const char8_t *data () const { return u8"bar"; }
  constexpr decltype (sizeof 0) size () const { return 3; }
};

consteval { __builtin_constexpr_diag (0); }			// { dg-error "wrong number of arguments to '__builtin_constexpr_diag' call" }
consteval { __builtin_constexpr_diag (0, ""); }			// { dg-error "wrong number of arguments to '__builtin_constexpr_diag' call" }
consteval { __builtin_constexpr_diag (0, "", "", ""); }		// { dg-error "wrong number of arguments to '__builtin_constexpr_diag' call" }
consteval { __builtin_constexpr_diag (3, "", ""); }		// { dg-error "first '__builtin_constexpr_diag' call argument should be 0, 1, 2, 16, 17 or 18" }
consteval { __builtin_constexpr_diag (-42, "", ""); }		// { dg-error "first '__builtin_constexpr_diag' call argument should be 0, 1, 2, 16, 17 or 18" }
consteval { __builtin_constexpr_diag (1, "abcdABCD_0189", ""); }// { dg-warning "constexpr message:  \\\[abcdABCD_0189\\\]" }
consteval { __builtin_constexpr_diag (2, "%+-", ""); }		// { dg-error "'__builtin_constexpr_diag' tag string contains '\\\%' character other than letters, digits or '_'" }
consteval { __builtin_constexpr_diag (0, u8"foo", "bar"); }	// { dg-error "request for member 'size' in" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (1, u8"foo", "bar"); }	// { dg-error "request for member 'size' in" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (2, u8"foo", "bar"); }	// { dg-error "request for member 'size' in" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (0, "foo", u8"bar"); }	// { dg-message "constexpr message: bar \\\[foo\\\]" }
consteval { __builtin_constexpr_diag (1, "foo", u8"bar"); }	// { dg-warning "constexpr message: bar \\\[foo\\\]" }
consteval { __builtin_constexpr_diag (2, "foo", u8"bar"); }	// { dg-error "constexpr message: bar \\\[foo\\\]" }
consteval { __builtin_constexpr_diag (0, A {}, "foo"); }	// { dg-error "'struct A' has no member named 'size'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (1, B {}, "foo"); }	// { dg-error "'struct B' has no member named 'data'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (2, C {}, "foo"); }	// { dg-error "call to non-'constexpr' function '\[^\n\r]* C::size\\\(\\\) const'" }
consteval { __builtin_constexpr_diag (0, D {}, "foo"); }	// { dg-error "call to non-'constexpr' function 'const char\\\* D::data\\\(\\\) const'" }
consteval { __builtin_constexpr_diag (1, E {}, "foo"); }	// { dg-error "'struct E' has no member named 'size'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (2, F {}, "foo"); }	// { dg-error "constexpr message: foo \\\[bar\\\]" }
consteval { __builtin_constexpr_diag (0, G {}, "foo"); }	// { dg-error "conversion from 'const char8_t\\\*' to 'const char\\\*' in a converted constant expression" }
// { dg-error "could not convert '<anonymous>.G::data\\\(\\\)' from 'const char8_t\\\*' to 'const char\\\*'" "" { target *-*-* } .-1 }
// { dg-error "constexpr string 'data\\\(\\\)' must be implicitly convertible to 'const char\\\*'" "" { target *-*-* } .-2 }
consteval { __builtin_constexpr_diag (0, "", A {}); }		// { dg-error "'struct A' has no member named 'size'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (1, "", B {}); }		// { dg-error "'struct B' has no member named 'data'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (2, "", C {}); }		// { dg-error "call to non-'constexpr' function '\[^\n\r]* C::size\\\(\\\) const'" }
consteval { __builtin_constexpr_diag (0, "", D {}); }		// { dg-error "call to non-'constexpr' function 'const char\\\* D::data\\\(\\\) const'" }
consteval { __builtin_constexpr_diag (1, "", E {}); }		// { dg-error "'struct E' has no member named 'size'" }
// { dg-error "constexpr string must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
consteval { __builtin_constexpr_diag (2, "", F {}); }		// { dg-error "constexpr message: bar" }
consteval { __builtin_constexpr_diag (0, "", G {}); }		// { dg-message "constexpr message: bar" }
