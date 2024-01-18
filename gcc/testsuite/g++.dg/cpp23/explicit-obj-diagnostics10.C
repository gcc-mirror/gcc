// PR c++/113389
// { dg-do compile { target c++23 } }

struct A {
  void foo(A, this A);	// { dg-error "only the first parameter" }
  void qux(A, this A,	// { dg-error "only the first parameter" }
	   this A);	// { dg-error "only the first parameter" }
};
