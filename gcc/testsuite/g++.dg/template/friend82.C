// { dg-do compile }

template<class T>
struct A {
  template<class U> struct B;

  template<class U>
  struct C {
    template<class V> friend struct A::B;
  private:
    int x;
  };
};

template <class T>
template <class U>
struct A<T>::B {
  int foo(A<int*>::C<long> c) { return c.x; }  // { dg-error "private" }
};

template struct A<int*>::C<long>;
template struct A<int*>::B<long>;  // { dg-bogus "" }
template struct A<double*>::B<long>;  // { dg-message "required from here" }
