// { dg-do assemble  }

struct A {
  int rep;
  static const A a(0); // { dg-error "" } initialization
  static const A b = 3; // { dg-error "" } initialization
  static const A& c = 2; // { dg-error "" } initialization
  A(int x) : rep(x) {}
};
