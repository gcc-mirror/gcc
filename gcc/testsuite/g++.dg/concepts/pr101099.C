// PR c++/101099
// { dg-do compile { target c++11 } }
// { dg-options "-fconcepts" }

#include <initializer_list>
constexpr auto list = { 1, 2, 3 };
