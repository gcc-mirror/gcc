// PR c++/28341

template<const int&> struct A {};

template<typename T> struct B
{
  A<(T)0> b; // { dg-error "constant" }
  A<T(0)> a; // { dg-error "constant" }
};

B<const int&> b;
