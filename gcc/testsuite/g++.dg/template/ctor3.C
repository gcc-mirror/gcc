struct A {};
struct B;

template <class TP> struct X: virtual A {
  template <class TP2> X(TP2* ptr) {}
  template <class TP2> X(const X<TP2>) {}
};

struct Y : X<B> { 
  Y(A* a) : X<B>(a) {}
};

void func1(X<B>);

void func2() {
  A a;
  Y y(&a);
  func1(X<A>(&a));
}
