// PR c++/37177
// { dg-options -std=c++0x }

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

  (void*)(&S::foo<int>);
  (void)(&S::foo<int>);
  decltype(&S::foo<int>) b;

  (void*)(&N1::foo<int>);
  (void)(&N1::foo<int>);
  decltype(&N1::foo<int>) c;

  (void*)(&foo<int>);
  (void)(&foo<int>);
  decltype(&foo<int>) d;

  &foo<int> == 0;
}
