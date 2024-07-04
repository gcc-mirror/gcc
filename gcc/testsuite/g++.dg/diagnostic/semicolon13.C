// DR 569, Spurious semicolons at namespace scope should be allowed
// PR c++/113760
// { dg-options "-Wpedantic" }

// C++11 allows extra semicolons at namespace scope.
struct S {
  void foo();
};
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }

void S::foo () {
};			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }

namespace N {
};			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }

void f();
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }

void
f ()
{
};			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }

int x;
;			// { dg-warning "extra .;. outside of a function" "" { target c++98_only } }
