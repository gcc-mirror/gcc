/* { dg-options "-Wshadow" } */

struct A {
  void a1 () {
    struct B { B() {} }; // There should be no warning here.
  }
  void a2 () {
      struct B { };
  }
};
