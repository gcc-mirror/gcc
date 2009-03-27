// { dg-do compile }

// These typename should work as they are types.
struct A
{
  typedef int a;
  template <int>
  struct f {};
  template<int> void foo(int i)
  {
    typename A::a(i1);
    typename A::f<0>(i2);
  }
};
