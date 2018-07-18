// PR c++/71747
// { dg-do compile { target c++11 } }

template < bool > struct A
{
  typedef int type; 
  constexpr bool operator() () const 
  { 
    return true; 
  }
}; 

template < bool, typename = int > struct F; 
template < bool X > 
// should be: struct F < X, typename A < A < X > {} () >::type > 
struct F < X, typename A < F < X > {} () >::type >
{
};

F < true > f;			// { dg-error "" }
