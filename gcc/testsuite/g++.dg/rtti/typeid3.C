#include <typeinfo> 
 
template <template <class> class T> struct A { 
    void error() { 
      typeid(T).name(); // { dg-error "missing" }
    } 
}; 
 
template <class T> struct B {}; 
 
template void A<B>::error();
