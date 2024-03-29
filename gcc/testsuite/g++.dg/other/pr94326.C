// PR c++/94326
// { dg-do compile { target c++11 } }
// { dg-options "-fcompare-debug" }

template <typename = int> struct A {
  const int &foo() { return 0; }	// { dg-message "returning reference to temporary" }
  template <typename _Kt> void bar(_Kt) { foo(); }
};
struct B {
  A<> b;
  template <typename _Kt> auto baz(_Kt p1) -> decltype(b.bar(p1)) {
    b.bar(p1);
  }
};
struct C {};
void operator<(C, int) {
  B a;
  a.baz(C{});
}
