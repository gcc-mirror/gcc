// PR c++/34238
// { dg-do compile }

namespace
{
  template <typename T = int> struct A
  {
    static const bool a = true;
  };
}
struct A<> a;
