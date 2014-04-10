// PR c++/52844
// { dg-do compile { target c++11 } }

template < class > struct V { };  
template < int...Is > void f ( V < Is...>) { }  // { dg-error "mismatch|type" }
auto g ( ) -> decltype ( f ( V < long >  ( ) ) ) ; // { dg-error "matching" }
