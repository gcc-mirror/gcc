// { dg-do compile { target c++11 } }

#include <initializer_list>

auto value = std::initializer_list<int>{ 1, 2, 3 };
