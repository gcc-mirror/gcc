// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "" }

int z[3];

void
foo ()
{
  template for (static auto a : {})		// { dg-warning "for-range-declaration cannot be 'static'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (thread_local auto a : {})	// { dg-warning "for-range-declaration cannot be 'thread_local'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (__thread auto a : {})		// { dg-warning "for-range-declaration cannot be '__thread'" }
    ;						// { dg-warning "function-scope 'a' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
  template for (register auto a : {})		// { dg-warning "for-range-declaration cannot be 'register'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (extern auto a : {})		// { dg-warning "for-range-declaration cannot be 'extern'" }
    ;						// { dg-error "'a' has both 'extern' and initializer" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
  template for (mutable auto a : {})		// { dg-error "non-member 'a' cannot be declared 'mutable'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (virtual auto a : {})		// { dg-error "'virtual' outside class declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (explicit auto a : {})		// { dg-error "'explicit' outside class declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (friend auto a : {})		// { dg-error "'friend' used outside of class" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (typedef auto a : {})		// { dg-error "typedef declared 'auto'" }
    ;						// { dg-error "typedef 'a' is initialized \\\(use 'decltype' instead\\\)" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
#if __cplusplus >= 202002L
  template for (consteval auto a : {})		// { dg-error "a variable cannot be declared 'consteval'" "" { target c++20 } }
    ;						// { dg-warning "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
  template for (constinit auto a : {})		// { dg-error "for-range-declaration cannot be 'constinit'" "" { target c++20 } }
    ;						// { dg-warning "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
#endif
  template for (inline auto a : {})		// { dg-error "'inline' specifier invalid for variable 'a' declared at block scope" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (struct S { int a; } a : {})	// { dg-error "types may not be defined in a for-range-declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (enum E { E0 } a : {})		// { dg-error "types may not be defined in a for-range-declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
}

void
bar ()
{
  template for (static auto a : z)		// { dg-warning "for-range-declaration cannot be 'static'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (thread_local auto a : z)	// { dg-warning "for-range-declaration cannot be 'thread_local'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (__thread auto a : z)		// { dg-warning "for-range-declaration cannot be '__thread'" }
    ;						// { dg-warning "function-scope 'a' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
  template for (register auto a : z)		// { dg-warning "for-range-declaration cannot be 'register'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (extern auto a : z)		// { dg-warning "for-range-declaration cannot be 'extern'" }
    ;						// { dg-error "'a' has both 'extern' and initializer" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
  template for (mutable auto a : z)		// { dg-error "non-member 'a' cannot be declared 'mutable'" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (virtual auto a : z)		// { dg-error "'virtual' outside class declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (explicit auto a : z)		// { dg-error "'explicit' outside class declaration" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (friend auto a : z)		// { dg-error "'friend' used outside of class" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (typedef auto a : z)		// { dg-error "typedef declared 'auto'" }
    ;						// { dg-error "typedef 'a' is initialized \\\(use 'decltype' instead\\\)" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
#if __cplusplus >= 202002L
  template for (consteval auto a : z)		// { dg-error "a variable cannot be declared 'consteval'" "" { target c++20 } }
    ;						// { dg-warning "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
  template for (constinit auto a : z)		// { dg-error "for-range-declaration cannot be 'constinit'" "" { target c++20 } }
    ;						// { dg-warning "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
#endif
  template for (inline auto a : z)		// { dg-error "'inline' specifier invalid for variable 'a' declared at block scope" }
    ;						// { dg-warning "'template for' only available with" "" { target c++23_down } .-1 }
  template for (struct S { int a; } a : z)	// { dg-error "types may not be defined in a for-range-declaration" }
    ;						// { dg-error "conversion from 'int' to non-scalar type 'bar\\\(\\\)::S' requested" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
						// { dg-error "conversion from 'int' to non-scalar type 'bar\\\(\\\)::S' requested" "" { target *-*-* } .-2 }
  template for (enum E { E0 } a : z)		// { dg-error "types may not be defined in a for-range-declaration" }
    ;						// { dg-error "invalid conversion from 'int' to 'bar\\\(\\\)::E'" "" { target *-*-* } .-1 }
						// { dg-warning "'template for' only available with" "" { target c++23_down } .-2 }
						// { dg-error "invalid conversion from 'int' to 'bar\\\(\\\)::E'" "" { target *-*-* } .-2 }
}
