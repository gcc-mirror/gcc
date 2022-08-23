// PR c++/101894

struct A
{
  template<int> friend void foo();
  template<int> friend void foo() {}
};
