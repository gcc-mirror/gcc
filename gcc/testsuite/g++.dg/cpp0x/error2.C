// PR c++/38656
// { dg-options "-std=c++11" }

template<int> int foo();

template<typename F> void bar(F f)
{
  f((foo<0>()=0)...); // { dg-error "pattern '\\(foo\\<0\\>\\)\\(\\)=0'" }
}
