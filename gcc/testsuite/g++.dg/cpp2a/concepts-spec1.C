// Example from CA 104 proposal.
// { dg-do compile { target concepts } }

template <class T> concept C = sizeof(T) == 8;
template <class T> struct A {
  template <class U> U f(U) requires C<typename T::type>; // #1
  template <class U> U f(U) requires C<T>; // #2
};

template <> template <class U> U A<int>::f(U) requires C<int> { } // OK, specializes #2
