// PR c++/81843
// { dg-do compile { target c++11 } }

template < typename > struct A;
template < typename, typename > struct B;
template < typename ... S > struct C
{
  template < typename > struct D {};
  template < typename ... T > struct D < A < B < S, T > ... > >;
};

C <>::D < A < B < int, int > > > c;
