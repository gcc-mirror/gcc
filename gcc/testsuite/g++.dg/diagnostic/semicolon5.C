// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "-Wextra-semi" }
// { dg-prune-output "only available with" }

struct X { ; };		      // { dg-warning "extra .;. inside a struct" }

struct S {
  void baz () = delete;
  void qux () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" }
  void corge () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" }
  ;			      // { dg-warning "extra .;. inside a struct" }
  int s;
  ;			      // { dg-warning "extra .;. inside a struct" }
};
