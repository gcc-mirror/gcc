// PR c++/29730

struct A
{
  template<int> void foo()(0); // { dg-error "initializer" } 
};
