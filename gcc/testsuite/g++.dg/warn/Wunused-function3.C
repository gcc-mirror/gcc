// { dg-options -Wunused-function }

namespace
{
  template <class T> struct A
  {
    friend void f(A) { }
  };

  A<int> a;
}
