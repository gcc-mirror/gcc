// P0634R3
// { dg-do compile { target c++2a } }
// { dg-options "-fconcepts" }

template <typename, typename> class A { class B; };

template <typename T, typename U> class A<T, U>::B {
  B(A &);
};

template <typename T, typename U>
A<T, U>::B::B(A<T, U> &) {}
