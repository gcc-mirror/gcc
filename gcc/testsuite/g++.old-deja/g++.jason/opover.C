// { dg-do assemble  }
// Bug: g++ prefers a non-matching operator== over user-defined conversions
// and a default operator==.

struct A {
  operator int ();
};

struct B {
  friend int operator== (B, int);
};

int foo (A& a) {
  return a == 1;
}
