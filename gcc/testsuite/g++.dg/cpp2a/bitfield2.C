// P0683R1
// { dg-do compile { target c++11 } }
// { dg-options "" }

int a;
const int b = 0;
struct T {
  int i : true ? 10 : b = 6;	// { dg-error "assignment of read-only variable" }
  int : 4 = 10;			// { dg-error "default member initializer for unnamed bit-field" }
  int : 5 = a + b;		// { dg-error "default member initializer for unnamed bit-field" }
};
template <bool V, int W>
struct U {
  int j : W = 7;		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int k : W { 8 };		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int l : V ? 7 : a = 3;	// { dg-error "modification of .a. from outside current evaluation is not a constant expression" }
				// { dg-error "width not an integer constant" "" { target *-*-* } .-1 }
  int m : (V ? W : b) = 9;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
				// { dg-error "zero width for bit-field" "" { target *-*-* } .-1 }
  int n : (V ? W : b) { 10 };	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
				// { dg-error "zero width for bit-field" "" { target *-*-* } .-1 }
  int o : 1 || new int { 0 };
  int : 4 = 10;			// { dg-error "default member initializer for unnamed bit-field" }
  int : 5 = a + b;		// { dg-error "default member initializer for unnamed bit-field" }
};
U<false, 10> u;
