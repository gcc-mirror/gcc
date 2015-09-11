// { dg-do compile }

// Origin: Ivan Godard <igodard@pacbell.net>
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/16175: Missing cv qualifier in error message output

template <typename> struct Template {}; 
 
template<template<typename> class D> 
struct B { 
    static void foo1(const D<void> *);	// { dg-message "declared" }
    static void foo2(volatile D<void> *);// { dg-message "declared" }
}; 
 
class E : protected B<Template> {}; 
 
void bar() {
  E::foo1 (0);				// { dg-error "context" }
  E::foo2 (0);				// { dg-error "context" }
}
