// { dg-options -Wsystem-headers }

#include <cstdlib>
#include <new>

void *operator new (std::size_t) throw (std::bad_alloc); // { dg-line decl }
// { dg-error "dynamic exception spec" "" { target c++17 } decl }
// { dg-warning "dynamic exception spec" "" { target { c++11 && { ! c++17 } } } decl }
// { dg-warning "different exception spec" "" { target { c++11 && { ! c++17 } } } decl }
