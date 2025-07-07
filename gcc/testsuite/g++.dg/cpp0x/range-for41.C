// PR c++/84009
// { dg-do compile { target c++11 } }
// { dg-options "" }

int z[64];

void
foo ()
{
  for (static auto a : z)		// { dg-warning "for-range-declaration cannot be 'static'" }
    ;
  for (thread_local auto a : z)		// { dg-warning "for-range-declaration cannot be 'thread_local'" }
    ;
  for (__thread auto a : z)		// { dg-warning "for-range-declaration cannot be '__thread'" }
    ;					// { dg-warning "function-scope 'a' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
  for (register auto a : z)		// { dg-warning "for-range-declaration cannot be 'register'" }
    ;					// { dg-warning "does not allow 'register' storage class specifier" "" { target c++17 } .-1 }
  for (extern auto a : z)		// { dg-warning "for-range-declaration cannot be 'extern'" }
    ;					// { dg-error "'a' has both 'extern' and initializer" "" { target *-*-* } .-1 }
  for (mutable auto a : z)		// { dg-error "non-member 'a' cannot be declared 'mutable'" }
    ;
  for (virtual auto a : z)		// { dg-error "'virtual' outside class declaration" }
    ;
  for (explicit auto a : z)		// { dg-error "'explicit' outside class declaration" }
    ;
  for (friend auto a : z)		// { dg-error "'friend' used outside of class" }
    ;
  for (typedef auto a : z)		// { dg-error "typedef declared 'auto'" }
    ;					// { dg-error "typedef 'a' is initialized \\\(use 'decltype' instead\\\)" "" { target *-*-* } .-1 }
#if __cplusplus >= 202002L
  for (consteval auto a : z)		// { dg-error "a variable cannot be declared 'consteval'" "" { target c++20 } }
    ;
  for (constinit auto a : z)		// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
    ;
#endif
  for (inline auto a : z)		// { dg-error "'inline' specifier invalid for variable 'a' declared at block scope" }
    ;
  for (struct S { int a; } a : z)	// { dg-error "types may not be defined in a for-range-declaration" }
    ;					// { dg-error "conversion from 'int' to non-scalar type 'foo\\\(\\\)::S' requested" "" { target *-*-* } .-1 }
  for (enum E { E0 } a : z)		// { dg-error "types may not be defined in a for-range-declaration" }
    ;					// { dg-error "invalid conversion from 'int' to 'foo\\\(\\\)::E'" "" { target *-*-* } .-1 }
}
