// { dg-options -std=c++17 }

template <class T> struct A {
  A(T); // #1
  A(const A&); // #2
};

template <class T> A(T) -> A<T>;  // #3

A a (42); // uses #3 to deduce A<int> and initializes with #1
A b = a;  // uses #2 (not #3) to deduce A<int> and initializes with #2; #2 is more specialized

template <class T> A(A<T>) -> A<A<T>>;  // #4

A b2 = a;  // uses #4 to deduce A<A<int>> and initializes with #1; #4 is as specialized as #2

template <class,class> struct same;
template <class T> struct same<T,T> {};

same<decltype(a),A<int>> s1;
same<decltype(b),A<int>> s2;
same<decltype(b2),A<A<int>>> s3;
