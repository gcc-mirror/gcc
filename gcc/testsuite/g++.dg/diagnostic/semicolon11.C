// DR 569, Spurious semicolons at namespace scope should be allowed
// PR c++/113760
// { dg-options "-pedantic-errors" }

// C++11 allows extra semicolons at namespace scope.
struct S {
  void foo();
};
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }

void S::foo () {
};			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }

namespace N {
};			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }

void f();
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }

void
f ()
{
};			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }

int x;
;			// { dg-error "extra .;. outside of a function" "" { target c++98_only } }
