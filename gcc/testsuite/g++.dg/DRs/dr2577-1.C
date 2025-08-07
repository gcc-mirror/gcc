// DR 2577 - Undefined behavior for preprocessing directives in macro arguments
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A(x)
#define B(x, y)
A(
#if 1				// { dg-error "embedding a directive within macro arguments is not portable" }
1
#else				// { dg-error "embedding a directive within macro arguments is not portable" }
2
#endif				// { dg-error "embedding a directive within macro arguments is not portable" }
)
B(1,
#line 234			// { dg-error "embedding a directive within macro arguments is not portable" }
)
#line 18
A(
#define C 1			// { dg-error "embedding a directive within macro arguments is not portable" }
)
A(
#undef C			// { dg-error "embedding a directive within macro arguments is not portable" }
)
B(42,
# 234 "dr2577-1.C"		// { dg-error "embedding a directive within macro arguments is not portable" }
)				// { dg-error "style of line directive is a GCC extension" "" { target *-*-* } .-1 }
#line 28 "dr2577-1.C"
B(
#warning "foobar"		// { dg-error "embedding a directive within macro arguments is not portable" }
, 12)				// { dg-error "'#warning' before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } .-1 }
				// { dg-warning "#warning \"foobar\"" "" { target *-*-* } .-2 }
A(
#pragma GCC diagnostics push	// { dg-error "embedding a directive within macro arguments is not portable" }
)
B(5,
#pragma GCC diagnostics pop	// { dg-error "embedding a directive within macro arguments is not portable" }
)
A(
#error foobar			// { dg-error "embedding a directive within macro arguments is not portable" }
)				// { dg-error "#error foobar" "" { target *-*-* } .-1 }
