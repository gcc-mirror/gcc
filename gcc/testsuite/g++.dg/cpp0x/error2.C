// PR c++/38656
// { dg-do compile { target c++11 } }

template<int> int foo();

template<typename F, int N> void bar(F f)
{
  f((foo<N>()=0)...); // { dg-error "pattern '\\(foo\\<N\\>\\)\\(\\)=0'" }
}
