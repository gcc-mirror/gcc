template <class R>
struct A {
  static int _test; // { dg-error "" }
  static int _test; // { dg-error "" }
};
template <class R> int A<R>::_test = 0;
struct B : public A <int> { };
