// PR c++/71834
// { dg-do compile { target c++11 } }

template < typename ... Ts > struct A 
{
  template < Ts ..., typename U > struct B {};
};

// should be, e.g.: A < int >::B < 0, int > e; 
A < int >::B < 0 > e;	   // { dg-error "wrong number of template arguments" }
