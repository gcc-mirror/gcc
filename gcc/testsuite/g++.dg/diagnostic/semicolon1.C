// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "" }
// { dg-prune-output "only available with" }

struct X { ; };

struct S {
  void baz () = delete;
  void qux () = delete;
  ;
  void corge () = delete;
  ;
  ;
  int s;
  ;
};
