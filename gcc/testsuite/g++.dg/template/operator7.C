//PR c++/27493

template<operator T> void foo()         // { dg-error "before|template" }
{
  struct A {};
}
