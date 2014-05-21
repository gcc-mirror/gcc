// PR c++/60373
// { dg-do compile { target c++11 } }
// { dg-require-visibility "" }

#include <new>
__attribute__((visibility("hidden")))void*operator new(std::size_t); // { dg-warning "visibility attribute ignored" }

// { dg-message "previous declaration" "" { target *-*-* } 128 }
