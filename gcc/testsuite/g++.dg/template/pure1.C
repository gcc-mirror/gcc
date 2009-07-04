// PR c++/26122

struct A
{
  template<int> void foo() = 1; // { dg-error "pure|non-virtual" }
};
