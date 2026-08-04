// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }

#include <typeinfo>

const std::type_info &ti = typeid(decltype(^^int));
int main () {}
