// PR c++/84936
// { dg-do compile { target c++11 } }

struct A
{
  template<typename... T> A(T... t)
    : decltype(t)() {} // { dg-error "parameter pack" }
};

A a;
