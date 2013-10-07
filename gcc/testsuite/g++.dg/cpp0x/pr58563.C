// PR c++/58563
// { dg-do compile { target c++11 } }

template<int> void foo()
{
  enum E {};
  E().E::~T(); // { dg-error "not a class" }
}
