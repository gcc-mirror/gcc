// Example 4 from [temp.res.general]/3.

struct A {
  struct X { };
  int X;
};
struct B {
  struct X { };
};
template<class T> void f(T t) {
  typename T::X x; // { dg-error "'int A::X', which is not a type" }
}
void foo() {
  A a;
  B b;
  f(b); // OK, T::X refers to B::X
  f(a); // error: T::X refers to the data member A::X not the struct A::X
  // { dg-message "required from here" "" { target *-*-* } .-1 }
}
