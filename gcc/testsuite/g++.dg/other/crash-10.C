// Origin: PR c++/43327
// { dg-do compile }

template <typename _T>
struct A
{
  template <int _N, int _M> struct B;

  template <int _N>
  struct B<_N, _T::m>
  {
     static void f();
  };
};

struct C
{
  static const int m = 4;
};

void m()
{
  A<C>::B<1, 4>::f();
}
