// PR c++/123347
// { dg-do compile { target c++14 } }

#include <typeinfo>

int a;
template <int N>
const std::type_info &v = typeid (a);
