// PR c++/60218
// { dg-do compile { target c++11 } }

template<typename...> struct A {};

template<typename T> struct B
{
  typedef A<__bases(T)...> C;	// { dg-error "incomplete type" }
};

struct X {};
struct Y : X* {};  // { dg-error "expected" }

B<Y> b;
