// PR c++/59096
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  typedef T B [[mode]];   // { dg-warning "ignored" }
};

A<int>::B b;
