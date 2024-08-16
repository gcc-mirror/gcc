// { dg-do compile { target c++11 } }

struct A {
  [[]];			// { dg-error "declaration does not declare anything" }
};
struct B {
  [[gnu::deprecated]];	// { dg-error "declaration does not declare anything" }
};
