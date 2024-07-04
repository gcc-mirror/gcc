// DR 569, Spurious semicolons at namespace scope should be allowed
// PR c++/113760
// { dg-options "-Wextra-semi" }

// C++11 allows extra semicolons at namespace scope.
struct S {
  void foo();
};
;			// { dg-warning "extra .;. outside of a function" }

void S::foo () {
};			// { dg-warning "extra .;. outside of a function" }
;			// { dg-warning "extra .;. outside of a function" }

namespace N {
};			// { dg-warning "extra .;. outside of a function" }
;			// { dg-warning "extra .;. outside of a function" }

void f();
;			// { dg-warning "extra .;. outside of a function" }

void
f ()
{
};			// { dg-warning "extra .;. outside of a function" }
;			// { dg-warning "extra .;. outside of a function" }

int x;
;			// { dg-warning "extra .;. outside of a function" }
