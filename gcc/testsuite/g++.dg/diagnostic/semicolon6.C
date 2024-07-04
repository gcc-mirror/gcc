// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "-Wextra-semi -pedantic-errors" }
// { dg-prune-output "only available with" }

struct X { ; };		      // { dg-warning "extra .;. inside a struct" "" { target c++11 } }
			      // { dg-error "extra .;. inside a struct" "" { target c++98_only } .-1 }

struct S {
  void baz () = delete;
  void qux () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++11 } }
			      // { dg-error "extra .;. inside a struct" "" { target c++98_only } .-1 }
  void corge () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++11 } }
			      // { dg-error "extra .;. inside a struct" "" { target c++98_only } .-1 }
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++11 } }
			      // { dg-error "extra .;. inside a struct" "" { target c++98_only } .-1 }
  int s;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++11 } }
			      // { dg-error "extra .;. inside a struct" "" { target c++98_only } .-1 }
};
