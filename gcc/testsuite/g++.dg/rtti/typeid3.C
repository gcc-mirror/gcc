#include <typeinfo> 
 
template <template <class> class T> struct A { 
    void error() { 
      typeid(T).name(); // { dg-error "" }
    } 
}; 
 
template <class T> struct B {}; 
 
template void A<B>::error(); // { dg-error "instantiated" }
