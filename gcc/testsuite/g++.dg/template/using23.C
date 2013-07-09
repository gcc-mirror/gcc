// PR c++/57831

struct A {
  void f();
};
template <class T> struct B : T {
  typedef T base;
  using base::f;         // If I write "using B<T>::f" it's ok
  void g( ) {
    B<T>::f();           // This is OK as expected
    (this->*&T::f)();    // This is also OK
    (this->*&B<T>::f)(); // This causes error
  }
};
template struct B< A >;
