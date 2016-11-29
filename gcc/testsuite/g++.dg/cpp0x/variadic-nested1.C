// PR c++/71833
// { dg-do compile { target c++11 } }

template < typename ... Ts > struct A 
{
  template < Ts ..., typename ... Us > struct B {};
};

A <>::B < int > e;
