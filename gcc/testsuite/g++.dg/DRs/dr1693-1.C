// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "" }

struct S {
  int a;
  ;
};
