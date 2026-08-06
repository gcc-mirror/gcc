// { dg-do compile }
// { dg-options "" }

struct S { int a, b, c; };

S a = { 1, 2, 3 };
S b = { .a = 1, .b = 2, .c = 3 };
S c = { 1, .b = 2, .c = 3 };	// { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target { c++20 && c++26_down } } }
				// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" "" { target c++20 } .-1 }
S d = { .a = 1, 2, 3 };		// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target { c++20 && c++26_down } } }
				// { dg-error "designated initializer clause should not be followed by non-designated" "" { target c++29 } .-1 }
S e = { .b = 1, .b = 2 };	// { dg-error "designator used multiple times in the same initializer list" }

#if __cplusplus > 201103L
template <int... N>
void
foo ()
{
  S f = { .a = N... };		// { dg-error "'...' not allowed in designated initializer list" "" { target c++20 } }
}
#endif
