struct A {
  static const int size = BOGUS; // { dg-error "" }
};
const int A::size;
