// PR c++/34100
// { dg-do compile }

template<typename T> struct A
{
  typedef typename T::X Y __attribute__((vector_size(8)));	// { dg-error "is not a class, struct" }
};

A<int> a;
