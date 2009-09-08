// PR c++/39923
// { dg-options "-std=c++0x" }

#include <initializer_list>

void test3()
{
  std::initializer_list<int> list{move}; // { dg-error "not declared|could not convert" }
}
