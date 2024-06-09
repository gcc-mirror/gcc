// DR 569, Spurious semicolons at namespace scope should be allowed
// PR c++/113760
// { dg-options "-Wextra-semi -pedantic-errors" }

// C++11 allows extra semicolons at namespace scope.
struct S {
  void foo();
};
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }

void S::foo () {
};			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }

namespace N {
};			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }

void f();
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }

void
f ()
{
};			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }

int x;
;			// { dg-warning "extra .;. outside of a function" "" { target c++11 } }
			// { dg-error "extra .;. outside of a function" "" { target c++98_only } .-1 }
