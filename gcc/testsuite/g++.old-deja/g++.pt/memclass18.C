// { dg-do assemble  }

template <class T> struct A {
  template <class U> struct B;
};

template <class T> template <class U> struct A<T>::B { };

A<int>::B<int> b;
