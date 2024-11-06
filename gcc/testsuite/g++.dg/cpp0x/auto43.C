// PR c++/59114
// { dg-do compile { target c++11 } }

template<int> struct A
{
  template<typename T> operator T();
};

void foo()
{
  A<0>().operator auto();  // { dg-error "auto" }
}
