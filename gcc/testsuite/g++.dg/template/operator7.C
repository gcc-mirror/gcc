//PR c++/27493
// { dg-options "-std=gnu++98" }

template<operator T> void foo()         // { dg-error "before|template" }
{
  struct A {};
}
