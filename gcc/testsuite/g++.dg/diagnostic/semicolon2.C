// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "-Wpedantic" }
// { dg-prune-output "only available with" }

struct X { ; };		      // { dg-warning "extra .;. inside a struct" "" { target c++98_only } }

struct S {
  void baz () = delete;
  void qux () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++98_only } }
  void corge () = delete;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++98_only } }
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++98_only } }
  int s;
  ;			      // { dg-warning "extra .;. inside a struct" "" { target c++98_only } }
};
