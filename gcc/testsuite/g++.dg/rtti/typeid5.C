// PR c++/29928
// { dg-do compile }

#include <typeinfo>

struct S;

void f()
{ 
  const std::type_info& info1 = typeid(int []);
  const std::type_info& info2 = typeid(S [3]);
  const std::type_info& info3 = typeid(S []);
}
