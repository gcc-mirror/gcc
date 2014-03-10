// PR c++/37177
// { dg-do compile { target c++11 } }

#include <typeinfo>

namespace N1
{
  template<class T> bool foo();
}

struct S
{
  template <class T>
  static bool foo();

  template <class T>
  bool bar();
};

template<class T> bool foo();

int main()
{
  (void)(&S::bar<int>);
  decltype(&S::bar<int>) a;
  typeid(&S::bar<int>);

  (void*)(&S::foo<int>);
  (void)(&S::foo<int>);
  decltype(&S::foo<int>) b;
  typeid(&S::foo<int>);

  (void*)(&N1::foo<int>);
  (void)(&N1::foo<int>);
  decltype(&N1::foo<int>) c;
  typeid(&N1::foo<int>);

  (void*)(&foo<int>);
  (void)(&foo<int>);
  decltype(&foo<int>) d;
  typeid(&foo<int>);

  &foo<int> == 0;
}
