// PR c++/26558
// Origin: Jan Gorski <slimak@yk74.internetdsl.tpnet.pl>
// { dg-do compile }

template<int> struct A
{
  template<int> void foo()
  {
    foo<0>::; // { dg-error "before|function template-id" }
  }
};
