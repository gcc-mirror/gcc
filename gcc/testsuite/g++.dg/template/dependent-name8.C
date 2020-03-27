// PR c++/94057 - template keyword in a typename-specifier.
// { dg-do compile }

template<typename T> struct A {
  template<typename U> struct B {
    B(typename A<T>::B<U>&);
    void fn(typename A<T>::B<U>);
  };
};
