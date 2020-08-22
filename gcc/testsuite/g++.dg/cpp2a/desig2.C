// { dg-do compile }
// { dg-options "" }

struct S { int a, b, c; };

S a = { 1, 2, 3 };
S b = { .a = 1, .b = 2, .c = 3 };
S c = { 1, .b = 2, .c = 3 };	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++20 } }
S d = { .a = 1, 2, 3 };		// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++20 } }
S e = { .b = 1, .b = 2 };	// { dg-error "designator used multiple times in the same initializer list" }

#if __cplusplus > 201103L
template <int... N>
void
foo ()
{
  S f = { .a = N... };		// { dg-error "'...' not allowed in designated initializer list" "" { target c++20 } }
}
#endif
