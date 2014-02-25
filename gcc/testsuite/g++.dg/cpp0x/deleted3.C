// PR c++/60216
// { dg-require-effective-target c++11 }

struct A
{
  template<typename T> A(T) = delete;
};

template<> A::A<int>(int) {}

A a(0);
