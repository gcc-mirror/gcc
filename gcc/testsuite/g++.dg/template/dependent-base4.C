// PR c++/117993

template<class T>
struct A {
  void f();
  typedef void type;
};

template<class T>
struct B : A<T> {
  template<class U> struct C;
};

template<class T>
template<class U>
struct B<T>::C : B {
  void g(C& c) {
    this->f();           // { dg-bogus "member" }
    c.f();               // { dg-bogus "member" }
    C::f();              // { dg-bogus "member" }
    typename C::type* p; // { dg-bogus "not name a type" }
  }
};
