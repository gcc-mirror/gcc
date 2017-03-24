// PR c++/77339
// { dg-do compile { target c++11 } }

template < typename > using A = int;

//OK: template < typename X > A < X > a; 
template < typename X > A < X >::a; // { dg-error "" }
