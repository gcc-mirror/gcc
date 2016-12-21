// PR c++/78767
// { dg-do compile { target c++11 } }

template <class T> struct A
{
  template <class U>
  A(U, U = 42);
};

struct B: A<int>
{
  using A::A;
};

B b(24);
