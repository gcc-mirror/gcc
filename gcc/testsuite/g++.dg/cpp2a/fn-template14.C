// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<typename> struct B
{
  template<typename> int foo() { return 0; }
  int i = foo<int>();
};
