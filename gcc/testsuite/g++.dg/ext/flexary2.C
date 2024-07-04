// PR c++/46688 - [4.6 Regression] g++ requires a function declaration
// when it should not
// Note that although the definition of struct B in the test case for
// c++/46688 was thought to be valid, it is, in fact, invalid, in C and
// as noted in c++/42121, should be treated as invalid in C++ as well.
// The test verifies that gcc detects and reports the right error.

// { dg-options "" }

struct A {
  A(int);
};

struct B {
  B() {}
  A a[];
};

struct C {
  C() {}
  A a[0];  // -Wpedantic warning: ISO C++ forbids zero-size arrays
};

