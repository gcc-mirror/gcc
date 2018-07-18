// { dg-do compile { target c++11 } }
// PR C++/71710 ICE with decltype & using

template < typename > struct A
{
  A a;
  template < int > using B = decltype (a);
  B < 0 > b;
  template < int C > B < C > foo ();
};
