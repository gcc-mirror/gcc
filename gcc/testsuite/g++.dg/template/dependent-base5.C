template<class T>
struct A { };

template<class T>
struct B : A<T> {
  template<class U> struct C;
};

struct D { void f(); };

template<class T>
template<class U>
struct B<T>::C : B {
  void g() {
    D::f(); // { dg-bogus "without object" }
  }
};

template<>
struct A<int> : D { };

template struct B<int>::C<int>;
