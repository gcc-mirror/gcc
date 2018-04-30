// { dg-options -std=c++17 }

template <class T> struct A {
  using value_type = T;
  A(value_type); // #1
  A(const A&); // #2
  A(T, T, int); // #3
  template<class U> A(int, T, U); // #4
}; // A(A); #5, the copy deduction candidate

A x (1, 2, 3); // uses #3, generated from a non-template constructor

template <class T> A(T) -> A<T>;  // #6, less specialized than #5

A a (42); // uses #6 to deduce A<int> and #1 to initialize
A b = a;  // uses #5 to deduce A<int> and #2 to initialize

template <class T> A(A<T>) -> A<A<T>>;  // #7, as specialized as #5

A b2 = a;  // uses #7 to deduce A<A<int>> and #1 to initialize

template <class,class> struct same;
template <class T> struct same<T,T> {};

same<decltype(a),A<int>> s1;
same<decltype(b),A<int>> s2;
same<decltype(b2),A<A<int>>> s3;
