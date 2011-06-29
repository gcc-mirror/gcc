// { dg-do compile }
// Contributed by: <fasbjx at free dot fr>
// PR c++/14389: Disambiguate overloaded member templates which differ only
//  in the template argument list.

namespace N1 {

struct S { 
    template< typename B, typename A > void foo(); 
    template< typename A >             void foo(); 
}; 
 
template< typename A >             void S::foo() {} 
template< typename B, typename A > void S::foo() {} 
 
template void S::foo<void> (); 
template void S::foo<void,void> (); 

}

namespace N2 {

struct S { 
  template< typename _A > void foo(); 
  template< int _i >      void foo(); 
}; 

template< typename _A > void S::foo() {} 

template void S::foo< 0 >();    // { dg-error "no definition available" "no def" }
				// { dg-message "required" "instantiated" { target *-*-* } 30 }

}
