// PR c++/24915

struct A
{
  template<int> void foo() {}
  template<int> int foo() { return 0; }
};
