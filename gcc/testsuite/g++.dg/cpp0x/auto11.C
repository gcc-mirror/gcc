// PR c++/38256
// { dg-options "-std=c++11" }

template<int> struct A
{
  template<typename T> operator T();
};

void foo()
{
  A<0>().operator auto();	// { dg-error "auto.*conversion" }
}
