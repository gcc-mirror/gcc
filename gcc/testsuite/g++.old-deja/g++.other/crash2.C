// Build don't link:

struct A {
  int rep;
  static const A a(0); // ERROR - initialization
  static const A b = 3; // ERROR - initialization
  static const A& c = 2; // ERROR - initialization
  A(int x) : rep(x) {}
};
