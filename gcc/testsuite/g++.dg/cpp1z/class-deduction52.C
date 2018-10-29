// PR c++/84768
// { dg-do compile { target c++17 } }

template<typename> struct A {};

template<typename T> struct B
{
  template<X Y> B(A<T>);  // { dg-error "declared" }
};

B b = A<void>();
