// PR c++/79501
// { dg-do compile { target c++17 } }

template<class T>
struct A {
  template<class U> struct B { template<class V> B(V); };
  B(T) -> B<T>;
};

A<int>::B b(0);
