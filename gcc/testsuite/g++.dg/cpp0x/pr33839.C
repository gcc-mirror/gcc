// { dg-options -std=c++11 }
template<int> struct A;

void foo()
{
  __decltype A<0>; // { dg-error "invalid declarator|expected" }
  __decltype (A<0>); // { dg-error "must be an expression" }
}
