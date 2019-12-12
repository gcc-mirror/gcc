// PR c++/88554
// { dg-do compile }
// { dg-options "-Wreturn-type" }

struct X {
  friend X & operator+= (X &, int) { }	// { dg-warning "no return statement in function returning non-void" }
					// { dg-bogus "return \\*this;" "" { target *-*-* } .-1 }
};
struct Y {};
Y & operator += (Y &, Y &) { }		// { dg-warning "no return statement in function returning non-void" }
					// { dg-bogus "return \\*this;" "" { target *-*-* } .-1 }
