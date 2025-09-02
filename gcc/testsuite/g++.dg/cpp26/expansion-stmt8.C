// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }

struct S { int y; } z[3];

template <int N>
void
foo ()
{
  template for (static auto [ a ] : {})		// { dg-error "for-range-declaration cannot be 'static'" }
    ;						// { dg-error "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-2 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-3 }
  template for (thread_local auto [ a ] : {})	// { dg-error "for-range-declaration cannot be 'thread_local'" }
    ;						// { dg-error "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-2 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-3 }
  template for (__thread auto [ a ] : {})	// { dg-error "for-range-declaration cannot be '__thread'" }
    ;						// { dg-error "function-scope 'structured binding' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
						// { dg-error "structured binding declaration can be '__thread' only in" "" { target c++17_down } .-2 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-3 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-4 }
  template for (register auto [ a ] : {})	// { dg-error "structured binding declaration cannot be 'register'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (extern auto [ a ] : {})		// { dg-error "structured binding declaration cannot be 'extern'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (mutable auto [ a ] : {})	// { dg-error "structured binding declaration cannot be 'mutable'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (virtual auto [ a ] : {})	// { dg-error "'virtual' outside class declaration" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (explicit auto [ a ] : {})	// { dg-error "'explicit' outside class declaration" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (friend auto [ a ] : {})		// { dg-error "'friend' used outside of class" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (typedef auto [ a ] : {})	// { dg-error "structured binding declaration cannot be 'typedef'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
#if __cplusplus >= 202002L
  template for (consteval auto [ a ] : {})	// { dg-error "structured binding declaration cannot be 'consteval'" "" { target c++20 } }
    ;						// { dg-error "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
  template for (constinit auto [ a ] : {})	// { dg-error "for-range-declaration cannot be 'constinit'" "" { target c++20 } }
    ;						// { dg-error "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
#endif
  template for (inline auto [ a ] : {})		// { dg-error "structured binding declaration cannot be 'inline'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
}

template <int N>
void
bar ()
{
  template for (static auto [ a ] : z)		// { dg-error "for-range-declaration cannot be 'static'" }
    ;						// { dg-error "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-2 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-3 }
  template for (thread_local auto [ a ] : z)	// { dg-error "for-range-declaration cannot be 'thread_local'" }
    ;						// { dg-error "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-2 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-3 }
  template for (__thread auto [ a ] : z)	// { dg-error "for-range-declaration cannot be '__thread'" }
    ;						// { dg-error "function-scope 'structured binding' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
						// { dg-error "structured binding declaration can be '__thread' only in" "" { target c++17_down } .-2 }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-3 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-4 }
  template for (register auto [ a ] : z)	// { dg-error "structured binding declaration cannot be 'register'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (extern auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'extern'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (mutable auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'mutable'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (virtual auto [ a ] : z)		// { dg-error "'virtual' outside class declaration" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (explicit auto [ a ] : z)	// { dg-error "'explicit' outside class declaration" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (friend auto [ a ] : z)		// { dg-error "'friend' used outside of class" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
  template for (typedef auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'typedef'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
#if __cplusplus >= 202002L
  template for (consteval auto [ a ] : z)	// { dg-error "structured binding declaration cannot be 'consteval'" "" { target c++20 } }
    ;						// { dg-error "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
  template for (constinit auto [ a ] : z)	// { dg-error "for-range-declaration cannot be 'constinit'" "" { target c++20 } }
    ;						// { dg-error "'template for' only available with" "" { target { c++20 && c++23_down } } .-1 }
#endif
  template for (inline auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'inline'" }
    ;						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
						// { dg-error "'template for' only available with" "" { target c++23_down } .-2 }
}

void
baz ()
{
  foo <0> ();
  bar <0> ();
}
