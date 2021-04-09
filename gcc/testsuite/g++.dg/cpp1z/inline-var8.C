// PR c++/97975
// { dg-do compile { target c++17 } }

template <class>
class A
{
  static const float b;
  static inline const int c = b;
};

A<int> a;

struct B
{
  static const float b;
  static inline const int c = b;
};
