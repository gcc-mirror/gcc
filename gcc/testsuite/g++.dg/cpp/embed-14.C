// { dg-do compile }
// { dg-options "" }

struct S { int a; long b; unsigned char c[63]; int d; };
S s = {
#embed __FILE__ limit (64) prefix (.a = 1, .b = ) suffix (, .d = 2)	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++20 } }
};
const unsigned char t[66] = {
#embed __FILE__ limit (64) prefix ([0] = 1, [1] =) suffix (, [65] = 2)	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++20 } }
};
int u[] = { [0] =
#embed __FILE__ limit (64)						// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++20 } }
};
