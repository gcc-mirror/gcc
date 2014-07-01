// PR c++/59637
// { dg-do compile { target c++11 } }

template<typename T> void foo(T* p)
{
  p->decltype(T{})::~X();
}
