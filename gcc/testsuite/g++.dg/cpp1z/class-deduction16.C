// { dg-options -std=c++1z }

template <class... T>
struct A
{
  template <class...Us> A(Us&&...);
};

A a(1,2);
