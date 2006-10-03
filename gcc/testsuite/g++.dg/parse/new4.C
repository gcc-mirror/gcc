// PR c++/29291
// { dg-do compile }

template<int> void foo()
{
  new int(;  // { dg-error "before" }
}
