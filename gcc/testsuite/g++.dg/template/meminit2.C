// { dg-do compile }
// Origin: Mark Anders <mark dot a dot anders at intel dot com>
// PR c++/15503: disambiguators in base classes and mem-initializers

template <typename K1> struct O {  
  template <typename K2> struct I {}; 
}; 

template <typename T> 
struct A : typename O<T>::template I<int> {   // { dg-error "keyword 'typename' not allowed" }
  A() :    typename O<T>::template I<int>()   // { dg-error "keyword 'typename' not allowed" }
  {}
};

template <typename T> 
struct B : O<T>::template I<int> {
  B() :    O<T>::I<int>()   // { dg-error "used as template|it is a template" }
  {}
};

// { dg-bogus "end of input" "bogus token skipping in the parser" { xfail *-*-* } 17 }
