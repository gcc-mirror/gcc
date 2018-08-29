// PR c++/85146
// { dg-do compile { target c++11 } }

template<typename...> struct A {};

template<typename T> struct B
{
  typedef A<__direct_bases(T)...> C;	// { dg-error "incomplete type" }
};

struct X;

B<X> b;
