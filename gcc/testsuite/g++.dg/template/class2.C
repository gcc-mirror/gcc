// PR c++/13451

template <class T>
struct A {
  struct B;
  struct A::B { }; // { dg-error "" }
};
