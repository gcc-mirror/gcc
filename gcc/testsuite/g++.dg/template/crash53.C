// PR c++/28110
// { dg-do compile }

template<int> struct A {};

template<typename T> struct B
{
  template<T I> B(A<I>);  // { dg-error "template non-type parameter" }
};

B<double> a=A<0>();  // { dg-error "non-scalar type" }
