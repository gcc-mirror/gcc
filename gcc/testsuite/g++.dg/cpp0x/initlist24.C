// PR c++/39923
// { dg-do compile { target c++11 } }

#include <initializer_list>

void test3()
{
  std::initializer_list<int> list{move}; // { dg-error "not declared|could not convert" }
}
