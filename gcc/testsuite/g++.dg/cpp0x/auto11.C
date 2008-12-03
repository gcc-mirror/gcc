// PR c++/38256
// { dg-options "-std=c++0x" }

template<int> struct A
{
  template<typename T> operator T();
};

void foo()
{
  A<0>().operator auto();	// { dg-error "auto.*conversion" }
}
