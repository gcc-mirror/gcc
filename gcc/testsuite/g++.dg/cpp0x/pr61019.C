// PR c++/61019
// { dg-do compile { target c++11 } }

template<class>
struct S
{
  static_assert(((S*)0)->~S(), "");  // { dg-error "incomplete type" }
};

S<int> b;
