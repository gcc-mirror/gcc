// PR c++/84009
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int y; } z[64];

void
foo ()
{
  for (static auto [ a ] : z)		// { dg-warning "for-range-declaration cannot be 'static'" }
    ;					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
  for (thread_local auto [ a ] : z)	// { dg-warning "for-range-declaration cannot be 'thread_local'" }
    ;					// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
  for (__thread auto [ a ] : z)		// { dg-warning "for-range-declaration cannot be '__thread'" }
    ;					// { dg-warning "function-scope 'structured binding' implicitly auto and declared '__thread'" "" { target *-*-* } .-1 }
 					// { dg-warning "structured binding declaration can be '__thread' only in" "" { target c++17_down } .-2 }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-3 }
  for (register auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'register'" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (extern auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'extern'" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (mutable auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'mutable'" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (virtual auto [ a ] : z)		// { dg-error "'virtual' outside class declaration" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (explicit auto [ a ] : z)		// { dg-error "'explicit' outside class declaration" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (friend auto [ a ] : z)		// { dg-error "'friend' used outside of class" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (typedef auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'typedef'" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
#if __cplusplus >= 202002L
  for (consteval auto [ a ] : z)	// { dg-error "structured binding declaration cannot be 'consteval'" "" { target c++20 } }
    ;
  for (constinit auto [ a ] : z)	// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
    ;
#endif
  for (inline auto [ a ] : z)		// { dg-error "structured binding declaration cannot be 'inline'" }
    ;					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}
