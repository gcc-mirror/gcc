// Bug: g++ prefers a non-matching operator== over user-defined conversions
// and a default operator==.
// Build don't link:

struct A {
  operator int ();
};

struct B {
  friend operator== (B, int);
};

int foo (A& a) {
  return a == 1;
}
