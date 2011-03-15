// PR 34758 Bad diagnostic for circular dependency in constructor default argument
// { dg-do compile }
// { dg-options "" }
struct A
{
  A (const A& = A()); // { dg-error "recursive evaluation of default argument" }
};


struct S {
  S(const S& = f()); // { dg-error "default argument\[^\n\]*which is not yet defined" }
  static const S& f(int i = 3);
};

struct J {
  J(const J& = f(2)); // { dg-error "default argument.*which is not yet defined" }
  static const J& f(int i = 3, int j = 4);
};

struct Z {
  Z(const Z& = f(4));
  static const Z& f(int i = 3);
};

struct X {
  X(const X& = g());
  static const X& g(void);
};
