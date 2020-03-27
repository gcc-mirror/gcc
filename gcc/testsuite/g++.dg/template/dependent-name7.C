// PR c++/94057 - template keyword in a typename-specifier.
// { dg-do compile }

template<typename T> struct A {
  template<typename U> struct B {
    B(A<T>::B<U>&);
    void fn(A<T>::B<U>);
  };
};
