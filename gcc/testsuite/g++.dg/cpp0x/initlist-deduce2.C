// Test for PR63149
// { dg-do compile { target c++11 } }

#include <initializer_list>

const auto r = { 1, 2, 3 };
using X = decltype(r);
using X = const std::initializer_list<int>;
