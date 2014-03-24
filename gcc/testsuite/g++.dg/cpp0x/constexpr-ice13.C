// PR c++/59571
// { dg-do compile { target c++11 } }

template <class>
struct foo
{
  static constexpr int bar{(int)-1};
};
